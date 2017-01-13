(module static-serve
    ()

  (import scheme chicken (chicken format))
  (use spiffy simple-directory-handler)

  (parameterize ((root-path ".")
                 (handle-directory simple-directory-handler)
                 (server-bind-address "0.0.0.0")
                 (access-log (current-error-port))
                 (error-log (current-error-port)))
    (printf "Server listening on ~A:~A~N" (server-bind-address) (server-port))
    (start-server)))
