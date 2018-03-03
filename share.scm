(module share ()

;; TODO upload form
;; TODO archive for directory download

(import scheme chicken tcp extras ports srfi-18 files posix)
(use spiffy intarweb uri-common args gochan)

(tcp-buffer-size 2048)

(define options
  (list (args:make-option (i ip) (required: "IP_ADDRESS")
                          "Listens on IP_ADDRESS"
          (server-bind-address arg))
        (args:make-option (p port) (required: "PORT")
                          "Listens on PORT"
          (ensure number? (server-port (string->number arg))
                  "PORT must be a valid port number"))
        (args:make-option (c count) (required: "N")
                          "Share the file N times"
          (ensure number? (max-connections (string->number arg))
                  "Count must be a number"))
        (args:make-option (h help) #:none "Displays this text"
          (usage))
        ))

(define *channel* (gochan 0))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options…] [file]")
      (newline)
      (print (args:usage options)))))

(define (main args)
  ;; Default values
  (server-bind-address #f)
  (server-port 8080)
  (max-connections 1)
  (root-path "/nonexistent")
  (mime-type-map '())
  (receive (options operands)
      (args:parse args options)
    (if (not (= (length operands) 1))
        (usage)
        (share (car operands)))))

(define (share path)
  (let ((dir (pathname-directory path))
        (file (pathname-strip-directory path)))
    (unless (file-read-access? path)
      (error "Unable to open file for reading" path))
    (change-directory (or dir "."))
    (vhost-map `((".*" . ,(download-handler file))))
    (thread-start! start-server)
    ;; TODO detect address bound
    (fprintf (current-error-port)
             "Server started on http://~A:~A~%"
             (or (server-bind-address)
                 "localhost")
             (server-port))
    (wait-for-completion (max-connections))
    (exit 0)))

;; FIXME this is not perfect but should work well enough™
;; TODO install an exception handler that doesn’t crash the program
(define (wait-for-completion count)
  (unless (zero? count)
    (let ((th (gochan-recv *channel*)))
      (thread-join! th)
      (wait-for-completion (sub1 count)))))

(define ((download-handler filename) continue)
  (let ((uri (request-uri (current-request))))
    (if (equal? (uri-path uri) (list '/ filename))
        (begin
          (root-path ".")
          (fprintf (current-error-port)
                   "Sending ~S to ~A~%" filename (remote-address))
          (gochan-send *channel* (current-thread))
          (continue))
        (with-headers `((location
                         ,(update-uri uri
                            path: (list '/ filename))))
          (lambda () (send-status 'see-other))))))

(cond-expand ((or compiling script)
              (main (command-line-arguments)))
      (else))
)
