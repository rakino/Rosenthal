;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils transformations)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu packages package-management)
  #:export (rosenthal-transformation-guix))


(define* (rosenthal-transformation-guix #:key (substitutes? #t)
                                        (channel? #t)
                                        (guix-source? #f))
  (define %rosenthal-signing-key
    (plain-file "rosenthal.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #374EC58F5F2EC0412431723AF2D527AD626B049D657B5633AAAEBC694F3E33F9#)))"))

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
       (modify-services (operating-system-user-services os)
         (guix-service-type
          config => (guix-configuration
                     (inherit config)
                     (channels
                      (let ((configured-channels
                             (guix-configuration-channels config)))
                        (if channel?
                            (cons %rosenthal-channel
                                  (or configured-channels %default-channels))
                            configured-channels)))
                     (guix
                      (if guix-source?
                          (guix-for-channels channels)
                          (guix-configuration-guix config)))
                     (authorized-keys
                      (cons %rosenthal-signing-key
                            (guix-configuration-authorized-keys config)))
                     (substitute-urls
                      (delete-duplicates
                       `(,@(guix-configuration-substitute-urls config)
                         ,@(if substitutes?
                               '("https://ci.boiledscript.com")
                               '())))))))))))
