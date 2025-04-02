;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal utils download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:export (go-vendored-fetch))

;; NOTE: This approach shouldn't be upstreamed.
(define* (go-vendored-fetch src hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (go
                             (module-ref
                              (resolve-interface '(gnu packages golang))
                              ;; Use latest Go in Guix.
                              'go-1.23))
                            (nss-certs
                             (module-ref
                              (resolve-interface '(gnu packages certs))
                              'nss-certs)))
  (gexp->derivation
   (or name
       (string-append (origin-file-name src) "-vendored"))
   (with-imported-modules %default-gnu-imported-modules
     #~(begin
         (use-modules (guix build gnu-build-system)
                      (guix build utils))
         (setenv "GOCACHE" "/tmp/go")
         (setenv "GOMODCACHE" "/tmp/gomod")
         (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs"))
         ;; Support Unicode in file name.
         (setlocale LC_ALL "C.UTF-8")

         ((assoc-ref %standard-phases 'unpack) #:source #+src)
         (invoke #+(file-append go "/bin/go") "mod" "vendor")
         (copy-recursively "." #$output)))
   #:system system
   #:hash-algo hash-algo
   #:hash hash
   ;; Is a directory.
   #:recursive? #t
   ;; Honor the user's proxy and locale settings.
   #:leaked-env-vars '("http_proxy" "https_proxy"
                       "LC_ALL" "LC_MESSAGES" "LANG"
                       "COLUMNS")
   #:local-build? #t))
