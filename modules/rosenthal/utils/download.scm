;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils download)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; Guix build systems
  #:use-module (guix build-system gnu)
  #:export (go-mod-vendor))

;;;
;;; ‘go mod vendor’ based fetcher
;;;

(define* (go-mod-vendor #:key go)
  (lambda* (src hash-algo hash #:optional name #:key (system (%current-system)))
    (define nss-certs
      (module-ref (resolve-interface '(gnu packages nss)) 'nss-certs))

    (gexp->derivation
     (or name "vendored-go-dependencies")
     (with-imported-modules %default-gnu-imported-modules
       #~(begin
           (use-modules (guix build gnu-build-system)
                        (guix build utils))
           ;; Support Unicode in file name.
           (setlocale LC_ALL "C.UTF-8")
           ;; For HTTPS support.
           (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs"))

           ((assoc-ref %standard-phases 'unpack) #:source #+src)
           (invoke #+(file-append go "/bin/go") "mod" "vendor")
           (copy-recursively "vendor" #$output)))
     #:system system
     #:hash-algo hash-algo
     #:hash hash
     ;; Is a directory.
     #:recursive? #t
     #:env-vars '(("GOCACHE" . "/tmp/go-cache")
                  ("GOPATH" . "/tmp/go"))
     ;; Honor the user's proxy and locale settings.
     #:leaked-env-vars '("GOPROXY"
                         "http_proxy" "https_proxy"
                         "LC_ALL" "LC_MESSAGES" "LANG"
                         "COLUMNS")
     #:local-build? #t)))
