;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils transformations)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (rosenthal services file-systems)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages file-systems)
  #:export (rosenthal-transformation-guix
            rosenthal-transformation-zfs))


(define* (rosenthal-transformation-guix #:key (substitutes? #t)
                                        (channel? #t)
                                        (guix-source? #f))
  (define %rosenthal-signing-keys
    (list (plain-file "gokuraku.pub"
            "(public-key (ecc (curve Ed25519) (q #374EC58F5F2EC0412431723AF2D527AD626B049D657B5633AAAEBC694F3E33F9#)))")
          (plain-file "nuporta.pub"
            "(public-key (ecc (curve Ed25519) (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)))")))

  (define %rosenthal-channel
    (channel
      (name 'rosenthal)
      (url "https://codeberg.org/hako/rosenthal.git")
      (branch "trunk")
      (introduction
       (make-channel-introduction
        "7677db76330121a901604dfbad19077893865f35"
        (openpgp-fingerprint
         "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7")))))

  (lambda (os)
    (operating-system
      (inherit os)
      (services
       (cons* (simple-service 'guix-moe guix-service-type
                (guix-extension
                  (authorized-keys %rosenthal-signing-keys)
                  (substitute-urls '("https://cache-cdn.guix.moe"))))

              (modify-services (operating-system-user-services os)
                (guix-service-type
                 config => (guix-configuration
                             (inherit config)
                             (channels
                              (let ((configured-channels
                                     (guix-configuration-channels config)))
                                (if channel?
                                    (cons %rosenthal-channel
                                          (or configured-channels
                                              %default-channels))
                                    configured-channels)))
                             (guix
                              (if guix-source?
                                  (guix-for-channels channels)
                                  (guix-configuration-guix config)))))))))))

;; NOTE: Booting from ZFS requires patching Guix.
(define* (rosenthal-transformation-zfs #:key boot? (config (zfs-configuration)))
  (lambda (os)
    (operating-system
      (inherit os)
      (kernel-loadable-modules
       `(,@(if boot?
               `((,(package/inherit zfs
                     (arguments
                      (substitute-keyword-arguments (package-arguments zfs)
                        ((#:linux _ #f) (operating-system-kernel os)))))
                  "module"))
               '())
         ,@(operating-system-kernel-loadable-modules os)))
      (services
       (cons* (service zfs-service-type config)
              (operating-system-user-services os))))))
