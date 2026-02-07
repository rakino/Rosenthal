;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2023 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services dns)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  ;; Guix packages
  #:use-module (gnu packages dns)
  #:export (smartdns-configuration
            smartdns-service-type))

;;;
;;; Smartdns
;;;


(define-configuration smartdns-configuration
  (smartdns
   (file-like smartdns)
   "The Smartdns package.")
  (config-file
   (file-like (plain-file "empty" ""))
   "Configuration file for Smartdns.")
  (no-serialization))

(define smartdns-shepherd-service
  (match-record-lambda <smartdns-configuration>
      (smartdns config-file)
    (list (shepherd-service
           (documentation "Run smartdns.")
           (provision '(smartdns dns))
           (requirement '(loopback networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append smartdns "/sbin/smartdns")
                           "-f" "-c" #$(file-append config-file))))
           (stop #~(make-kill-destructor))))))

(define smartdns-service-type
  (service-type
   (name 'smartdns)
   (extensions
    (list (service-extension shepherd-root-service-type
                             smartdns-shepherd-service)))
   (default-value (smartdns-configuration))
   (description "Run Smartdns daemon.")))
