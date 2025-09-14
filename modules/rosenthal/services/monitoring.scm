;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services monitoring)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils serializers ini)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (rosenthal packages binaries)
  #:export (grafana-service-type
            grafana-configuration))

;;;
;;; Grafana
;;;

(define-configuration/no-serialization grafana-configuration
  (grafana
   (file-like grafana-bin)
   "")
  (config
   ini-config
   "")
  (database-password-file
   string
   "")
  (shepherd-provision
   (list-of-symbols '(grafana))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define grafana-account
  (lambda _
    (list (user-group (name "grafana") (system? #t))
          (user-account
            (name "grafana")
            (group "grafana")
            (system? #t)
            (comment "Grafana user")
            (home-directory "/var/lib/grafana")))))

(define grafana-postgresql-role
  (match-record-lambda <grafana-configuration>
      (database-password-file)
    (list (postgresql-role
            (name "grafana")
            (create-database? #t)
            (password-file database-password-file)))))

(define grafana-activation
  (match-record-lambda <grafana-configuration>
      (grafana)
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "grafana")))
          (for-each
           (lambda (directory)
             (unless (file-exists? directory)
               (mkdir-p directory)
               (chown directory (passwd:uid user) (passwd:gid user))))
           '("/var/log/grafana" "/var/lib/grafana"))))))

(define grafana-shepherd
  (match-record-lambda <grafana-configuration>
      (grafana config shepherd-provision shepherd-requirement auto-start?)
    (let ((config-file
           (apply mixed-text-file "grafana.ini" (ini-serialize config))))
      (list (shepherd-service
              (provision shepherd-provision)
              (requirement `(loopback postgresql user-processes
                             ,@shepherd-requirement))
              (start
               #~(make-forkexec-constructor
                  (list #$(file-append grafana "/bin/grafana")
                        "server" "--config" #$config-file)
                  #:user "grafana"
                  #:group "grafana"
                  #:directory #$(file-append grafana "/share/grafana"))))))))

(define grafana-service-type
  (service-type
    (name 'grafana)
    (extensions
     (list (service-extension account-service-type
                              grafana-account)
           (service-extension postgresql-role-service-type
                              grafana-postgresql-role)
           (service-extension activation-service-type
                              grafana-activation)
           (service-extension shepherd-root-service-type
                              grafana-shepherd)))
    (description "")))
