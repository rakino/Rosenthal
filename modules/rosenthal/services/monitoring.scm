;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services monitoring)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils serializers ini)
  #:use-module (rosenthal utils serializers yaml)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages guile-xyz)
  #:use-module (rosenthal packages binaries)
  #:export (grafana-service-type
            grafana-configuration

            prometheus-service-type
            prometheus-configuration))

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
                  #:directory #$(file-append grafana "/share/grafana")))
              (stop #~(make-kill-destructor))
              (auto-start? auto-start?))))))

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

;;;
;;; prometheus
;;;

(define-configuration/no-serialization prometheus-configuration
  (prometheus
   (file-like prometheus-bin)
   "")
  (listen-address
   (string "0.0.0.0:9090")
   "")
  (config
   yaml-config
   "")
  (shepherd-provision
   (list-of-symbols '(prometheus))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define prometheus-account
  (lambda _
    (list (user-group (name "prometheus") (system? #t))
          (user-account
            (name "prometheus")
            (group "prometheus")
            (system? #t)
            (comment "Prometheus user")
            (home-directory "/var/lib/prometheus")))))

(define prometheus-activation
  (match-record-lambda <prometheus-configuration>
      (prometheus)
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "prometheus"))
              (directory "/var/lib/grafana"))
          (unless (file-exists? directory)
            (mkdir-p directory)
            (chown directory (passwd:uid user) (passwd:gid user)))))))

(define prometheus-shepherd
  (match-record-lambda <prometheus-configuration>
      (prometheus listen-address config shepherd-provision shepherd-requirement auto-start?)
    (let ((config-file
           (computed-file "prometheus.yml"
             (with-extensions (list guile-yamlpp)
               #~(begin
                   (use-modules (yamlpp))
                   (call-with-output-file #$output
                     (lambda (port)
                       (let ((emitter (make-yaml-emitter)))
                         (yaml-emit! emitter '#$config)
                         (display (yaml-emitter-string emitter) port)))))))))
      (list (shepherd-service
              (provision shepherd-provision)
              (requirement `(loopback user-processes ,@shepherd-requirement))
              (start
               #~(make-forkexec-constructor
                  (list #$(file-append prometheus "/bin/prometheus")
                        (string-append "--config.file=" #$config-file)
                        (string-append "--web.listen-address=" #$listen-address))
                  #:user "prometheus"
                  #:group "prometheus"
                  #:directory "/var/lib/prometheus"
                  #:log-file "/var/log/prometheus.log"))
              (stop #~(make-kill-destructor))
              (auto-start? auto-start?))))))

(define prometheus-service-type
  (service-type
    (name 'prometheus)
    (extensions
     (list (service-extension account-service-type
                              prometheus-account)
           (service-extension activation-service-type
                              prometheus-activation)
           (service-extension shepherd-root-service-type
                              prometheus-shepherd)))
    (description "")))
