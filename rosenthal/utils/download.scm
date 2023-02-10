;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal utils download)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:export (url-fetch/xz-file))

(define url-fetch* (@@ (guix download) url-fetch*))

(define* (url-fetch/xz-file url hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (guile (default-guile)))
  "Similar to 'url-fetch' but decompress the xz file at URL as the result.
This is mainly used for adding xz-compressed patches to a origin definition.

\".xz\" extension in the URL is assumed."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))
  (define xz
    (module-ref (resolve-interface '(gnu packages compression)) 'xz))

  (mlet %store-monad ((drv (url-fetch* url hash-algo hash
                                       (or name (basename file-name ".xz"))
                                       #:system system
                                       #:guile guile))
                      (guile (package->derivation guile system)))
    ;; Take the xz file, and simply decompress it.
    ;; Use ungrafted xz so that the resulting file doesn't depend on whether
    ;; grafts are enabled.
    (gexp->derivation (or name file-name)
                      (with-imported-modules '((guix build utils))
                        #~(begin
                            (use-modules (guix build utils))
                            (setenv "XZ_OPT"
                                    (string-join (%xz-parallel-args)))

                            (copy-file #$drv #$file-name)
                            (make-file-writable #$file-name)
                            (invoke (string-append #+xz "/bin/unxz")
                                    #$file-name)

                            (copy-file (basename #$file-name ".xz")
                                       #$output)))
                      #:system system
                      #:guile-for-build guile
                      #:graft? #f
                      #:local-build? #t)))
