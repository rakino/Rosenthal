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
  #:export (alloy-configuration
            alloy-service-type

            grafana-service-type
            grafana-configuration

            loki-service-type
            loki-configuration

            mimir-service-type
            mimir-configuration

            prometheus-service-type
            prometheus-configuration))

;;;
;;; alloy
;;;

(define-configuration/no-serialization alloy-configuration
  (alloy
   (file-like alloy-bin)
   "")
  (config
   file-like
   "")
  (shepherd-provision
   (list-of-symbols '(alloy))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define alloy-activation
  (lambda _
    #~(begin
        (use-modules (guix build utils))
        (let ((directory "/var/lib/alloy"))
          (unless (file-exists? directory)
            (mkdir-p directory)
            (chmod directory #o755))))))

(define alloy-shepherd
  (match-record-lambda <alloy-configuration>
      (alloy config shepherd-provision shepherd-requirement auto-start?)
    (list (shepherd-service
            (provision shepherd-provision)
            (requirement `(loopback user-processes ,@shepherd-requirement))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append alloy "/bin/alloy") "run" #$config)
                #:directory "/var/lib/alloy"
                #:log-file "/var/log/alloy.log"))
            (stop #~(make-kill-destructor))
            (auto-start? auto-start?)))))

(define alloy-service-type
  (service-type
    (name 'alloy)
    (extensions
     (list (service-extension activation-service-type
                              alloy-activation)
           (service-extension shepherd-root-service-type
                              alloy-shepherd)))
    (description "")))


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
  (lambda _
    (list (postgresql-role
            (name "grafana")
            (create-database? #t)))))

(define grafana-activation
  (lambda _
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
                  #:directory #$(file-append grafana "/share/grafana")
                  #:log-file "/var/log/grafana.log"))
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
;;; loki
;;;

(define-configuration/no-serialization loki-configuration
  (loki
   (file-like loki-bin)
   "")
  (config
   yaml-config
   "")
  (shepherd-provision
   (list-of-symbols '(loki))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define loki-account
  (lambda _
    (list (user-group (name "loki") (system? #t))
          (user-account
            (name "loki")
            (group "loki")
            (system? #t)
            (comment "Loki user")
            (home-directory "/var/lib/loki")))))

(define loki-activation
  (lambda _
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "loki"))
              (directory "/var/lib/loki"))
          (unless (file-exists? directory)
            (mkdir-p directory)
            (chown directory (passwd:uid user) (passwd:gid user))
            (chmod directory #o755))))))

(define loki-shepherd
  (match-record-lambda <loki-configuration>
      (loki config shepherd-provision shepherd-requirement auto-start?)
    (let ((config-file
           (computed-file "loki.yaml"
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
                  (list #$(file-append loki "/bin/loki")
                        (string-append "-config.file=" #$config-file))
                  #:user "loki"
                  #:group "loki"
                  #:directory "/var/lib/loki"
                  #:log-file "/var/log/loki.log"))
              (stop #~(make-kill-destructor))
              (auto-start? auto-start?))))))

(define loki-service-type
  (service-type
    (name 'loki)
    (extensions
     (list (service-extension account-service-type
                              loki-account)
           (service-extension activation-service-type
                              loki-activation)
           (service-extension shepherd-root-service-type
                              loki-shepherd)))
    (description "")))


;;;
;;; mimir
;;;

(define-configuration/no-serialization mimir-configuration
  (mimir
   (file-like mimir-bin)
   "")
  (config
   yaml-config
   "")
  (shepherd-provision
   (list-of-symbols '(mimir))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define mimir-account
  (lambda _
    (list (user-group (name "mimir") (system? #t))
          (user-account
            (name "mimir")
            (group "mimir")
            (system? #t)
            (comment "Mimir user")
            (home-directory "/var/lib/mimir")))))

(define mimir-activation
  (lambda _
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "mimir"))
              (directory "/var/lib/mimir"))
          (unless (file-exists? directory)
            (mkdir-p directory)
            (chown directory (passwd:uid user) (passwd:gid user))
            (chmod directory #o755))))))

(define mimir-shepherd
  (match-record-lambda <mimir-configuration>
      (mimir config shepherd-provision shepherd-requirement auto-start?)
    (let ((config-file
           (computed-file "mimir.yaml"
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
                  (list #$(file-append mimir "/bin/mimir")
                        (string-append "-config.file=" #$config-file))
                  #:user "mimir"
                  #:group "mimir"
                  #:directory "/var/lib/mimir"
                  #:log-file "/var/log/mimir.log"))
              (stop #~(make-kill-destructor))
              (auto-start? auto-start?))))))

(define mimir-service-type
  (service-type
    (name 'mimir)
    (extensions
     (list (service-extension account-service-type
                              mimir-account)
           (service-extension activation-service-type
                              mimir-activation)
           (service-extension shepherd-root-service-type
                              mimir-shepherd)))
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
  (lambda _
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "prometheus"))
              (directory "/var/lib/prometheus"))
          (unless (file-exists? directory)
            (mkdir-p directory)
            (chown directory (passwd:uid user) (passwd:gid user))
            (chmod directory #o775))))))

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
