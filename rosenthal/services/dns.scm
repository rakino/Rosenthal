;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services dns)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (rosenthal packages dns)
  #:export (smartdns-configuration
            smartdns-service-type))

;;
;; Smartdns
;;

(define-configuration/no-serialization smartdns-configuration
  (smartdns
   (package smartdns)
   "The Smartdns package.")
  (config
   (file-like (plain-file "empty" ""))
   "Configuration file for Smartdns.")
  (log-file
   (string "/var/log/smartdns.log")
   "Where the logs go."))

(define smartdns-shepherd-service
  (match-lambda
    (($ <smartdns-configuration> smartdns config log-file)
     (list (shepherd-service
            (documentation "Run smartdns.")
            (provision '(smartdns dns))
            (requirement '(loopback networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append smartdns "/sbin/smartdns")
                            "-f" "-x" "-c" #$(file-append config))
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor)))))))

(define smartdns-service-type
  (service-type
   (name 'smartdns)
   (extensions
    (list (service-extension shepherd-root-service-type
                             smartdns-shepherd-service)))
   (default-value (smartdns-configuration))
   (description "Run Smartdns daemon.")))
