(module share ()
  (import scheme chicken extras)
  (use spiffy simple-directory-handler)

  (define (main args)
    (parameterize ((root-path ".")
                   (handle-directory simple-directory-handler)
                   (server-bind-address "0.0.0.0")
                   (access-log (current-error-port))
                   (error-log (current-error-port)))
      (printf "Server listening on ~A:~A~N"
              (server-bind-address)
              (server-port))
      (start-server)))

  (cond-expand (compiling (main (command-line-arguments)))
               (else))
)
