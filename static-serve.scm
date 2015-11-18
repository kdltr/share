(module static-serve
    ()

  (import scheme chicken extras)
  (use spiffy simple-directory-handler)

  (parameterize ((root-path ".")
                 (handle-directory simple-directory-handler)
                 (server-bind-address "0.0.0.0"))
    (printf "Server listening on ~A:~A~N" (server-bind-address) (server-port))
    (start-server)))
