;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services mail)
  ;; Guile builtins
  #:use-module (srfi srfi-26)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils serializers yaml)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu services docker)
  #:use-module (gnu services shepherd)
  ;; Guix Home - services
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  ;; Guix packages
  #:use-module (gnu packages mail)
  #:export (docker-mailserver-configuration
            docker-mailserver-service-type

            home-goimapnotify-service-type
            home-goimapnotify-configuration))

;;;
;;; Docker Mailserver
;;; https://docker-mailserver.github.io/docker-mailserver/latest/
;;;

(define-configuration docker-mailserver-configuration
  (data-directory
   (string "/var/lib/docker-mailserver")
   "Directory to store Docker Mailserver data.")
  (log-file
   (string "/var/log/docker-mailserver.log")
   "Path to log file.")
  (shepherd-requirement
   (list-of-symbols '())
   "List of Shepherd service dependencies.")
  (options
   (alist '())
   "Alist of Docker Mailserver configuration.  See also
@url{https://docker-mailserver.github.io/docker-mailserver/latest/config/environment/}.")
  (extra-arguments
   (list '())
   "List of extra Docker arguments.")
  (no-serialization))

(define docker-mailserver-oci
  (match-record-lambda <docker-mailserver-configuration>
      (data-directory log-file shepherd-requirement options extra-arguments)
    (let ((docker-mailserver-path
           (cut string-append data-directory <>)))
      (oci-extension
        (containers
         (list (oci-container-configuration
                 (environment options)
                 (image "ghcr.io/docker-mailserver/docker-mailserver:latest")
                 (provision "docker-mailserver")
                 (requirement shepherd-requirement)
                 (log-file log-file)
                 (network "host")
                 (volumes
                  `((,(docker-mailserver-path "/data") . "/var/mail")
                    (,(docker-mailserver-path "/state") . "/var/mail-state")
                    (,(docker-mailserver-path "/logs") . "/var/log/mail")
                    (,(docker-mailserver-path "/config") . "/tmp/docker-mailserver")
                    ("/etc/localtime" . "/etc/localtime:ro")))
                 (extra-arguments extra-arguments))))))))

(define docker-mailserver-service-type
  (service-type
   (name 'docker-mailserver)
   (extensions
    (list (service-extension oci-service-type
                             docker-mailserver-oci)))
   (default-value (docker-mailserver-configuration))
   (description "Run Docker Mailserver.")))


;;;
;;; goimapnotify
;;;

(define-configuration/no-serialization home-goimapnotify-configuration
  (goimapnotify
   (file-like goimapnotify)
   "")
  (config
   yaml-config
   "")
  (wait
   (integer 1)
   "")
  (shepherd-provision
   (list-of-symbols '(goimapnotify))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define home-goimapnotify-shepherd
  (match-record-lambda <home-goimapnotify-configuration>
      (goimapnotify config wait shepherd-provision shepherd-requirement auto-start?)
    (let ((config-file
           (mixed-text-file "goimapnotify.yaml" (yaml-serialize config))))
      (list (shepherd-service
              (provision shepherd-provision)
              (requirement shepherd-requirement)
              (start
               #~(make-forkexec-constructor
                  (list #$(file-append goimapnotify "/bin/goimapnotify")
                        "-conf" #$config-file
                        "-wait" (number->string #$wait))))
              (stop #~(make-kill-destructor))
              (auto-start? auto-start?))))))

(define home-goimapnotify-service-type
  (service-type
   (name 'home-goimapnotify)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-goimapnotify-shepherd)))
   (description "")))
