(module share ()

;; TODO archive for directory download
;; TODO directory/files listing mode


(import
  scheme
  (chicken base)
  (chicken file)
  (chicken file)
  (chicken file posix)
  (chicken format)
  (chicken pathname)
  (chicken port)
  (chicken process-context)
  (chicken tcp)
  srfi-1
  srfi-18
  args
  spiffy
  intarweb
  uri-common
  gochan
  multipart-form-data
  miscmacros)

(tcp-buffer-size 2048)

(define config-switches
  (list (args:make-option (i ip) (required: "IP_ADDRESS")
                          "Listens on IP_ADDRESS"
          (server-bind-address arg))
        (args:make-option (p port) (required: "PORT")
                          "Listens on PORT"
          (ensure number? (server-port (string->number arg))
                  "PORT must be a valid port number"))
        (args:make-option (c count) (required: "N")
                          "Close the server after N connections"
          (ensure number? (max-connections (string->number arg))
                  "Count must be a number"))
        (args:make-option (u upload) #:none
                          "Provide an upload form instead of a file")
        (args:make-option (h help) #:none "Displays this text"
          (usage) (exit))))

(define *main-channel* (gochan 0))
(define talkback-channel (make-parameter #f))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options…] [file]")
      (print "Usage: " (program-name) " [options…] -u")
      (newline)
      (print (args:usage config-switches)))))

(define (main args)
  ;; Default values
  (server-bind-address #f)
  (server-port 8080)
  (max-connections 1)
  (root-path "/nonexistent")
  (mime-type-map '())
  (receive (options operands)
      (args:parse args config-switches)
    (let ((download? (= (length operands) 1))
          (upload? (alist-ref 'upload options)))
      (cond ((and download? (not upload?))
             (provide-download (car operands)))
            ((and upload? (not download?))
             (provide-upload))
            (else
              (usage))))))

(define (startup!)
  (thread-start! start-server)
  ;; TODO detect address bound
  (fprintf (current-error-port)
           "Server started on http://~A:~A~%"
           (or (server-bind-address)
               "localhost")
           (server-port))
  (wait-for-completion (max-connections) '())
  (exit 0))

(define (provide-download path)
  (let ((dir (pathname-directory path))
        (file (pathname-strip-directory path)))
    (unless (regular-file? path)
      (error "You can only share regular files!"))
    (unless (file-readable? path)
      (error "Unable to open file for reading" path))
    (change-directory (or dir "."))
    (vhost-map `((".*" . ,(download-handler file))))
    (startup!)))

(define (provide-upload)
  (unless (file-writable? ".")
    (error "Unable to write any file in the current directory."))
  (vhost-map `((".*" . ,upload-handler)))
  (startup!))

(define (wait-for-completion count workers)
  (if (zero? count)
      (for-each gochan-recv workers)
      (let ((ch (gochan-recv *main-channel*)))
        (gochan-send ch #t)
        (wait-for-completion (sub1 count)
                             (cons ch workers)))))

(define (confirm-running)
  (let ((ch (gochan 1)))
    (gochan-send *main-channel* ch)
    (talkback-channel ch)
    (gochan-recv ch)))

(define (signal-end)
  (gochan-send (talkback-channel) #t))


(define ((download-handler filename) continue)
  (let ((uri (request-uri (current-request))))
    (if (equal? (uri-path uri) (list '/ filename))
        (begin
          (root-path ".")
          (confirm-running)
          (fprintf (current-error-port)
                   "Sending ~S to ~A~%" filename (remote-address))
          (dynamic-wind void continue signal-end))
        (with-headers `((location
                         ,(update-uri uri
                            path: (list '/ filename))))
          (lambda () (send-status 'see-other))))))

(define (upload-handler continue)
  (case (and (equal? (uri-path (request-uri (current-request)))
                     '(/ ""))
             (request-method (current-request)))
    ((GET) (upload-handler:GET continue))
    ((POST) (upload-handler:POST continue))
    (else (send-status 'not-implemented))))

(define (upload-handler:GET continue)
  (send-response body: (sprintf +html-template+ +form+)
                 headers: my-headers))

(define (upload-handler:POST continue)
  (unless
    (and-let* ((req (current-request))
               (content-type (header-value 'content-type
                                           (request-headers req)))
               (check (eq? content-type 'multipart/form-data))
               (_ (confirm-running))
               (data (read-multipart-form-data req))
               (upfile-field (alist-ref 'file data))
               (check (multipart-file? upfile-field))
               (filename (multipart-file-filename upfile-field))
               (port (multipart-file-port upfile-field))
               (local-filename (find-free-name filename)))
         (fprintf (current-error-port)
                  "Receiving ~S from ~A~%"
                  local-filename
                  (remote-address))
         (call-with-output-file local-filename
           (lambda (out) (copy-port port out)))
         (send-response body: (sprintf +html-template+
                                       "<p>Thanks a lot! :D</p>")
                        headers: my-headers)
         (signal-end)
         #t)
    (send-response body: (sprintf +html-template+
                                  "<p>Error! :o</p>")
                   headers: my-headers)))

(define (find-free-name name)
  (let ((name (pathname-file name))
        (ext (pathname-extension name)))
    (if (not (file-exists? (make-pathname "." name ext)))
        (make-pathname "." name ext)
        (let lp ((n 1))
          (let ((path (make-pathname
                        "."
                        (string-append name "." (number->string n))
                        ext)))
            (if (file-exists? path)
                (lp (add1 n))
                path))))))

(define my-headers
  '((content-type #(text/html ((charset . utf-8))))))

(define +html-template+ #<<EOF
<!DOCTYPE html>
<html>
<head><title>Share Upload</title></head>
<body>
~A
</body>
</html>
EOF
)

(define +form+ #<<EOF
<form name="upload" method="POST" enctype="multipart/form-data">
<p><input type="file" name="file" /></p>
<p><input type="submit" value="Upload!" /></p>
</form>
EOF
)

(define +thank+ #<<EOF
<p>Thanks a lot! :D</p>
EOF
)


(cond-expand ((or compiling script)
              (main (command-line-arguments)))
      (else))
)
