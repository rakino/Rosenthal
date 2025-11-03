;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services messaging)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (rosenthal packages messaging)
  #:export (heisenbridge-service-type
            heisenbridge-configuration

            mautrix-telegram-service-type
            mautrix-telegram-configuration))

;;;
;;; Heisenbridge
;;;

(define-configuration/no-serialization heisenbridge-configuration
  (heisenbridge
   (file-like heisenbridge)
   "")
  (homeserver
   (string "http://localhost:8008")
   "")
  (config
   file-like
   "")
  (shepherd-provision
   (list-of-symbols '(heisenbridge))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define heisenbridge-account
  (list (user-group (name "heisenbridge") (system? #t))
        (user-account
          (name "heisenbridge")
          (group "heisenbridge")
          (system? #t)
          (home-directory "/var/empty"))))

(define heisenbridge-shepherd
  (match-record-lambda <heisenbridge-configuration>
      (heisenbridge config homeserver shepherd-provision shepherd-requirement auto-start?)
    (list (shepherd-service
            (provision shepherd-provision)
            (requirement `(user-processes networking ,@shepherd-requirement))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append heisenbridge "/bin/heisenbridge")
                      "--config" #$config #$homeserver)
                #:user "heisenbridge"
                #:group "heisenbridge"
                #:environment-variables
                '("PATH=/run/current-system/profile/bin"
                  "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                  "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")))
            (stop #~(make-kill-destructor))
            (auto-start? auto-start?)
            (actions (list (shepherd-configuration-action config)))))))

(define heisenbridge-service-type
  (service-type
    (name 'heisenbridge)
    (extensions
     (list (service-extension account-service-type
                              (const heisenbridge-account))
           (service-extension shepherd-root-service-type
                              heisenbridge-shepherd)))
    (description "")))


;;;
;;; Mautrix Telegram
;;;

(define-configuration/no-serialization mautrix-telegram-configuration
  (mautrix-telegram
   (file-like mautrix-telegram)
   "")
  (config
   file-like
   "")
  (shepherd-provision
   (list-of-symbols '(mautrix-telegram))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define mautrix-telegram-account
  (list (user-group (name "mautrix") (system? #t))
        (user-account
          (name "mautrix-telegram")
          (group "mautrix")
          (system? #t)
          (home-directory "/var/lib/mautrix-telegram"))))

(define mautrix-telegram-activation
  (with-imported-modules (source-module-closure '((gnu build activation)))
    #~(begin
        (use-modules (gnu build activation))
        (let ((user (getpwnam "mautrix-telegram")))
          (mkdir-p/perms "/var/lib/mautrix-telegram" user #o700)))))

(define mautrix-telegram-postgresql-role
  (list (postgresql-role
          (name "mautrix-telegram")
          (create-database? #t))))

(define mautrix-telegram-shepherd
  (match-record-lambda <mautrix-telegram-configuration>
      (mautrix-telegram config shepherd-provision shepherd-requirement auto-start?)
    (list (shepherd-service
            (provision shepherd-provision)
            (requirement
             `(user-processes networking postgresql ,@shepherd-requirement))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append mautrix-telegram "/bin/mautrix-telegram")
                      "--no-update" "--config" #$config)
                #:user "mautrix-telegram"
                #:group "mautrix"
                #:directory "/var/lib/mautrix-telegram"))
            (stop #~(make-kill-destructor))
            (auto-start? auto-start?)
            (actions (list (shepherd-configuration-action config)))))))

(define mautrix-telegram-service-type
  (service-type
    (name 'mautrix-telegram)
    (extensions
     (list (service-extension account-service-type
                              (const mautrix-telegram-account))
           (service-extension activation-service-type
                              (const mautrix-telegram-activation))
           (service-extension postgresql-role-service-type
                              (const mautrix-telegram-postgresql-role))
           (service-extension shepherd-root-service-type
                              mautrix-telegram-shepherd)))
    (description "")))
