;;; SPDX-FileCopyrightText: 2024 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services web)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu services databases)
  #:use-module (gnu services docker)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages web)
  #:use-module (rosenthal utils serializers ini)
  #:use-module (rosenthal utils serializers yaml)
  #:export (caddy-configuration
            caddy-service-type

            forgejo-configuration
            forgejo-service-type

            iocaine-service-type
            iocaine-configuration

            jellyfin-configuration
            jellyfin-service-type

            komga-configuration
            komga-service-type

            misskey-configuration
            misskey-service-type

            navidrome-configuration
            navidrome-service-type

            vaultwarden-configuration
            vaultwarden-service-type))

;;;
;;; Caddy
;;;

(define-configuration/no-serialization caddy-configuration
  (caddy
   (file-like caddy)
   "")
  (caddyfile
   file-like
   "")
  ;; Shepherd
  (shepherd-provision
   (list-of-symbols '(caddy))
   "")
  (shepherd-requirement
   (list-of-symbols '())
   "")
  (auto-start?
   (boolean #t)
   ""))

(define (caddy-accounts config)
  (list (user-group (name "caddy") (system? #t))
        (user-account
         (name "caddy")
         (group "caddy")
         (system? #t)
         (comment "Caddy user")
         (home-directory "/var/lib/caddy"))))

(define caddy-privileged-programs
  (match-record-lambda <caddy-configuration>
      (caddy)
    (list (privileged-program
           (program (file-append caddy "/bin/caddy"))
           (capabilities "cap_net_bind_service=+ep")))))

(define (caddy-activation config)
  (with-imported-modules
      (source-module-closure '((guix build utils)
                               (gnu build activation)))
    #~(begin
        (use-modules (srfi srfi-26)
                     (guix build utils)
                     (gnu build activation))
        (let ((user (getpwnam "caddy")))
          (mkdir-p/perms "/var/lib/caddy" user #o750)
          (mkdir-p/perms "/var/log/caddy" user #o755)))))

(define caddy-etc
  (match-record-lambda <caddy-configuration>
      (caddyfile)
    `(("caddy/Caddyfile" ,caddyfile))))

(define caddy-shepherd-services
  (match-record-lambda <caddy-configuration>
      (shepherd-provision shepherd-requirement auto-start?)
    (list (shepherd-service
            (provision shepherd-provision)
            (requirement `(user-processes loopback ,@shepherd-requirement))
            (start
             #~(make-forkexec-constructor
                (list "/run/privileged/bin/caddy" "run"
                      "--environ" "--config" "/etc/caddy/Caddyfile")
                #:user "caddy"
                #:group "caddy"
                #:directory "/var/lib/caddy"
                #:log-file "/var/log/caddy.log"
                #:resource-limits '((nofile 1048576 1048576))
                #:environment-variables '("HOME=/var/lib/caddy")))
            (stop
             #~(make-kill-destructor))
            (actions
             (list (shepherd-configuration-action "/etc/caddy/Caddyfile")
                   (shepherd-action
                     (name 'reload)
                     (documentation "Reload Caddy configuration file.")
                     (procedure
                      #~(lambda (pid)
                          (if pid
                              (begin
                                (invoke "/run/privileged/bin/caddy" "reload"
                                        "--config" "/etc/caddy/Caddyfile")
                                (display "Service caddy has been asked to \
reload its configuration file."))
                              (display "Service caddy is not running.")))))))
            (auto-start? auto-start?)))))

(define caddy-service-type
  (service-type
   (name 'caddy)
   (extensions
    (list (service-extension account-service-type
                             caddy-accounts)
          (service-extension activation-service-type
                             caddy-activation)
          (service-extension etc-service-type
                             caddy-etc)
          (service-extension privileged-program-service-type
                             caddy-privileged-programs)
          (service-extension shepherd-root-service-type
                             caddy-shepherd-services)))
   (description "")))



;;;
;;; Forgejo
;;;


(define (file-object? val)
  (or (string? val)
      (file-like? val)))

(define list-of-file-likes?
  (list-of file-like?))

(define-configuration forgejo-configuration
  (forgejo
   (file-like forgejo)
   "Package to provide @file{/bin/forgejo}.")
  (git-packages
   (list-of-file-likes (list git git-lfs))
   "@code{git} and extension packages to install.")
  (config
   ini-config
   "")
  (postgresql-password-file
   string
   "")
  (no-serialization))

(define %forgejo-accounts
  (list (user-group (name "forgejo") (system? #t))
        (user-account
         (name "forgejo")
         (group "forgejo")
         (system? #t)
         (comment "Forgejo user")
         (home-directory "/var/lib/forgejo"))))

(define forgejo-postgresql-role
  (match-record-lambda <forgejo-configuration>
      (postgresql-password-file)
    (list (postgresql-role
            (name "forgejo")
            (create-database? #t)
            (password-file postgresql-password-file)))))

(define forgejo-activation
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "forgejo")))
        (for-each
         (lambda (dir)
           (mkdir-p dir)
           (chown dir (passwd:uid user) (passwd:gid user))
           (chmod dir #o750))
         '("/var/lib/forgejo" "/var/log/forgejo")))))

(define forgejo-shepherd-service
  (match-record-lambda <forgejo-configuration>
      (forgejo config)
    (let ((config-file
           (computed-file "forgejo.ini"
             (with-extensions (list guile-ini guile-lib guile-smc)
               #~(begin
                   (use-modules (srfi srfi-26) (ini))
                   (call-with-output-file #$output
                     (cut scm->ini '#$config #:port <>)))))))
      (list (shepherd-service
              (documentation "Run Forgejo.")
              (provision '(forgejo))
              (requirement '(loopback postgresql))
              (start
               #~(make-forkexec-constructor
                  (list #$(file-append forgejo "/bin/forgejo")
                        "--config" #$config-file)
                  #:user "forgejo"
                  #:group "forgejo"
                  #:log-file "/var/log/forgejo.log"
                  #:environment-variables
                  '("GIT_EXEC_PATH=/run/current-system/profile/libexec/git-core"
                    "GIT_SSL_CAINFO=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                    "HOME=/var/lib/forgejo"
                    "PATH=/run/current-system/profile/bin"
                    "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                    "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                  #:resource-limits '((nofile 524288 524288))))
              (stop
               #~(make-kill-destructor))
              (actions
               (list (shepherd-configuration-action config-file))))))))

(define forgejo-service-type
  (service-type
   (name 'forgejo)
   (extensions
    (list (service-extension account-service-type
                             (const %forgejo-accounts))
          (service-extension postgresql-role-service-type
                             forgejo-postgresql-role)
          (service-extension profile-service-type
                             (lambda (config)
                               (cons (forgejo-configuration-forgejo config)
                                     (forgejo-configuration-git-packages config))))
          (service-extension activation-service-type
                             (const forgejo-activation))
          (service-extension shepherd-root-service-type
                             forgejo-shepherd-service)))
   (description "Run Forgejo.")))


;;;
;;; Iocaine
;;;


(define-configuration/no-serialization iocaine-configuration
  (iocaine
   (file-like iocaine)
   "")
  (config
   file-object
   "")
  (log-file
   (string "/var/log/iocaine.log")
   "")
  (shepherd-provision
   (list-of-symbols '(iocaine))
   "")
  (shepherd-requirement
   (list-of-symbols '(loopback))
   "")
  (auto-start?
   (boolean #t)
   ""))

(define iocaine-accounts
  (list (user-group (name "iocaine") (system? #t))
        (user-account
          (name "iocaine")
          (group "iocaine")
          (system? #t)
          (comment "Iocaine user")
          (home-directory "/var/empty"))))

(define iocaine-etc
  (match-record-lambda <iocaine-configuration>
      (config)
    `(("iocaine/iocaine.toml" ,config))))

(define iocaine-shepherd-service
  (match-record-lambda <iocaine-configuration>
      (iocaine log-file shepherd-provision shepherd-requirement auto-start?)
    (list (shepherd-service
            (provision shepherd-provision)
            (requirement (cons 'user-processes shepherd-requirement))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append iocaine "/bin/iocaine")
                      "--config-file" "/etc/iocaine/iocaine.toml")
                #:user "iocaine"
                #:group "iocaine"
                #:log-file #$log-file))
            (stop #~(make-kill-destructor))
            (actions
             (list (shepherd-configuration-action "/etc/iocaine/iocaine.toml")
                   (shepherd-action
                     (name 'test)
                     (documentation "Test Iocaine configuration file.")
                     (procedure
                      #~(lambda (pid)
                          (if pid
                              (begin
                                (invoke #$(file-append iocaine "/bin/iocaine")
                                        "--config-file" "/etc/iocaine/iocaine.toml"
                                        "test")
                                (display "Service iocaine has been asked to \
test its configuration file."))
                              (display "Service iocaine is not running.")))))))
            (auto-start? auto-start?)))))

(define iocaine-service-type
  (service-type
   (name 'iocaine)
   (extensions
    (list (service-extension account-service-type
                             (const iocaine-accounts))
          (service-extension etc-service-type
                             iocaine-etc)
          (service-extension shepherd-root-service-type
                             iocaine-shepherd-service)
          (service-extension log-rotation-service-type
                             (compose list iocaine-configuration-log-file))))
   (description "")))


;;;
;;; Jellyfin
;;;


(define-maybe string)

(define-configuration jellyfin-configuration
  (cache-directory
   (string "/var/cache/jellyfin")
   "Path to cache directory.")
  (config-directory
   (string "/var/lib/jellyfin")
   "Path to configuration directory.")
  (proxy-url
   maybe-string
   "Proxy URL.")
  (log-file
   (string "/var/log/jellyfin.log")
   "Path to log file.")
  (auto-start?
   (boolean #t)
   "Whether to start automatically.")
  (extra-options
   (list '())
   "List of extra options.")
  (no-serialization))

(define %jellyfin-accounts
  (list (user-account
         (name "jellyfin")
         (group "docker")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define jellyfin-activation
  (match-record-lambda <jellyfin-configuration>
      (cache-directory config-directory)
    #~(let ((user (getpwnam "jellyfin")))
        (for-each
         (lambda (directory)
           (unless (file-exists? directory)
             (mkdir-p directory)
             (chown directory (passwd:uid user) (passwd:gid user))))
         '#$(list cache-directory config-directory)))))

(define jellyfin-oci
  (match-record-lambda <jellyfin-configuration>
      (cache-directory config-directory
                       proxy-url log-file auto-start? extra-options)
    (oci-extension
      (containers
       (list (oci-container-configuration
               (user "jellyfin")
               (group "docker")
               (environment
                (if (maybe-value-set? proxy-url)
                    `(("http_proxy" . ,proxy-url)
                      ("https_proxy" . ,proxy-url))
                    '()))
               (image "jellyfin/jellyfin:latest")
               (provision "jellyfin")
               (log-file log-file)
               (auto-start? auto-start?)
               (respawn? #t)
               (network "host")
               (volumes
                `((,cache-directory . "/cache")
                  (,config-directory . "/config")))
               (extra-arguments extra-options)))))))

(define jellyfin-service-type
  (service-type
   (name 'jellyfin)
   (extensions
    (list (service-extension account-service-type
                             (const %jellyfin-accounts))
          (service-extension activation-service-type
                             jellyfin-activation)
          (service-extension log-rotation-service-type
                             (compose list jellyfin-configuration-log-file))
          (service-extension oci-service-type
                             jellyfin-oci)))
   (default-value (jellyfin-configuration))
   (description "Run Jellyfin, a media system.")))


;;;
;;; Komga
;;;


(define-configuration komga-configuration
  (komga
   (file-like komga-bin)
   "Package to provide @file{/bin/komga}.")
  (port
   (integer 25600)
   "Port to listen to for the API and web interface.")
  (auto-start?
   (boolean #t)
   "Whether to start automatically.")
  (no-serialization))

(define %komga-accounts
  (list (user-group (name "komga") (system? #t))
        (user-account
         (name "komga")
         (group "komga")
         (system? #t)
         (comment "Komga user")
         (home-directory "/var/lib/komga"))))

(define komga-shepherd-service
  (match-record-lambda <komga-configuration>
      (komga port auto-start?)
    (list (shepherd-service
           (documentation "Run Komga.")
           (provision '(komga))
           (requirement '(loopback))
           (start
            #~(make-forkexec-constructor
               (list #$(file-append komga "/bin/komga"))
               #:user "komga"
               #:group "komga"
               #:log-file "/var/log/komga.log"
               #:environment-variables
               '("KOMGA_CONFIGDIR=/var/lib/komga"
                 #$(string-append "SERVER_PORT=" (number->string port)))))
           (stop
            #~(make-kill-destructor))
           (auto-start? auto-start?)))))

(define komga-service-type
  (service-type
   (name 'komga)
   (extensions
    (list (service-extension account-service-type
                             (const %komga-accounts))
          (service-extension shepherd-root-service-type
                             komga-shepherd-service)))
   (default-value (komga-configuration))
   (description "Run Komga.")))


;;;
;;; Misskey
;;;


(define-configuration misskey-configuration
  (image
   (string "misskey/misskey:latest")
   "Misskey docker image to use.")
  (config
   yaml-config
   "Alist of Misskey configuration, to be serialized to YAML format.")
  (data-directory
   (string "/var/lib/misskey")
   "Directory to store @file{files} in.")
  (log-file
   (string "/var/log/misskey.log")
   "Log file to use.")
  (postgresql-password-file
   string
   "")
  (no-serialization))

(define %misskey-accounts
  (list (user-account
         (name "misskey")
         (group "docker")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define misskey-postgresql-role
  (match-record-lambda <misskey-configuration>
      (postgresql-password-file)
    (list (postgresql-role
            (name "misskey")
            (create-database? #t)
            (password-file postgresql-password-file)))))

(define misskey-activation
  (match-record-lambda <misskey-configuration>
      (data-directory)
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "misskey")))
          (unless (file-exists? #$data-directory)
            (mkdir-p #$data-directory)
            (chown #$data-directory (passwd:uid user) (passwd:gid user)))))))

(define misskey-oci
  (match-record-lambda <misskey-configuration>
      (image config data-directory log-file )
    (let ((config-file
           (computed-file "misskey.yaml"
             (with-extensions (list guile-yamlpp)
               #~(begin
                   (use-modules (yamlpp))
                   (call-with-output-file #$output
                     (lambda (port)
                       (let ((emitter (make-yaml-emitter)))
                         (yaml-emit! emitter '#$config)
                         (display (yaml-emitter-string emitter) port)))))))))
      (oci-extension
        (containers
         (list (oci-container-configuration
                 (user "misskey")
                 (group "docker")
                 (image image)
                 (provision "misskey")
                 (requirement '(postgresql redis))
                 (log-file log-file)
                 (respawn? #t)
                 (network "host")
                 (volumes
                  `((,(string-append data-directory "/files") . "/misskey/files")
                    (,config-file . "/misskey/.config/default.yml"))))))))))

(define misskey-service-type
  (service-type
   (name 'misskey)
   (extensions
    (list (service-extension account-service-type
                             (const %misskey-accounts))
          (service-extension postgresql-role-service-type
                             misskey-postgresql-role)
          (service-extension log-rotation-service-type
                             (compose list misskey-configuration-log-file))
          (service-extension activation-service-type
                             misskey-activation)
          (service-extension oci-service-type
                             misskey-oci)))
   (description "Run Misskey, an interplanetary microblogging platform.")))


;;;
;;; Navidrome
;;;


(define-configuration navidrome-configuration
  (navidrome
   (file-like navidrome-bin)
   "")
  (ffmpeg
   (file-like ffmpeg)
   "")
  (auto-start?
   (boolean #t)
   "")
  (extra-config
   (string "")
   "")
  (no-serialization))

(define %navidrome-accounts
  (list (user-group (name "navidrome") (system? #t))
        (user-account
         (name "navidrome")
         (group "navidrome")
         (system? #t)
         (comment "Navidrome user")
         (home-directory "/var/lib/navidrome"))))

(define navidrome-shepherd-service
  (match-record-lambda <navidrome-configuration>
      (navidrome ffmpeg auto-start? extra-config)
    (let ((config-file
           (mixed-text-file
            "navidrome.toml"
            "DataFolder = '/var/lib/navidrome'\n"
            "CacheFolder = '/var/lib/navidrome/cache'\n"
            "EnableInsightsCollector = false\n"
            extra-config)))
      (list (shepherd-service
             (documentation "Run Navidrome.")
             (provision '(navidrome))
             (requirement '(loopback user-processes))
             (start
              #~(make-forkexec-constructor
                 (list #$(file-append navidrome "/bin/navidrome")
                       "--configfile" #$config-file)
                 #:user "navidrome"
                 #:group "navidrome"
                 #:log-file "/var/log/navidrome.log"
                 #:environment-variables
                 (list "LC_ALL=C.UTF-8"
                       (string-append "PATH=" #$ffmpeg "/bin"))))
             (stop
              #~(make-kill-destructor))
             (auto-start? auto-start?)
             (actions
              (list (shepherd-configuration-action config-file))))))))

(define navidrome-service-type
  (service-type
   (name 'navidrome)
   (extensions
    (list (service-extension account-service-type
                             (const %navidrome-accounts))
          (service-extension shepherd-root-service-type
                             navidrome-shepherd-service)))
   (default-value (navidrome-configuration))
   (description "Run Navidrome.")))


;;;
;;; Vaultwarden
;;;


(define-configuration vaultwarden-configuration
  (admin-token
   maybe-string
   "Token for the admin interface, preferably an Argon2 PCH string.")
  (database-url
   (string "postgresql://user:password@host:port/database")
   "Database URL.")
  (port
   (integer 8000)
   "Port to listen on.")
  (data-directory
   (string "/var/lib/vaultwarden")
   "Main data folder.")
  (log-file
   (string "/var/log/vaultwarden.log")
   "Logging to this file.")
  (proxy-url
   maybe-string
   "Proxy URL to use.")
  (extra-options
   (alist '())
   "Extra options.")
  (postgresql-password-file
   string
   "")
  (no-serialization))

(define %vaultwarden-accounts
  (list (user-account
         (name "vaultwarden")
         (group "docker")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define vaultwarden-postgresql-role
  (match-record-lambda <vaultwarden-configuration>
      (postgresql-password-file)
    (list (postgresql-role
            (name "vaultwarden")
            (create-database? #t)
            (password-file postgresql-password-file)))))

(define vaultwarden-activation
  (match-record-lambda <vaultwarden-configuration>
      (data-directory log-file)
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "vaultwarden")))
          (unless (file-exists? #$data-directory)
            (mkdir-p #$data-directory)
            (chown #$data-directory (passwd:uid user) (passwd:gid user)))
          (unless (file-exists? #$log-file)
            (mkdir-p (dirname #$log-file))
            (call-with-output-file #$log-file
              (lambda (port)
                (write-char #\newline port)))
            (chown #$log-file (passwd:uid user) (passwd:gid user)))))))

(define vaultwarden-oci
  (match-record-lambda <vaultwarden-configuration>
      (admin-token database-url port data-directory log-file proxy-url extra-options)
    (oci-extension
      (containers
       (list (oci-container-configuration
               (user "vaultwarden")
               (group "docker")
               (host-environment
                `(,@(if (maybe-value-set? admin-token)
                        `(("ADMIN_TOKEN" . ,admin-token))
                        '())
                  ("DATABASE_URL" . ,database-url)))
               (environment
                `(,@(if (maybe-value-set? proxy-url)
                        `(("HTTP_PROXY" . ,proxy-url))
                        '())
                  ("LOG_FILE" . "vaultwarden.log")
                  ("ROCKET_PORT" . ,(number->string port))
                  ("USE_SYSLOG" . "True")
                  ,@extra-options))
               (image "vaultwarden/server:latest-alpine")
               (provision "vaultwarden")
               (requirement '(postgresql))
               (respawn? #t)
               (network "host")
               (volumes
                `((,data-directory . "/data")
                  (,log-file . "/vaultwarden.log")))
               (extra-arguments
                `(,@(if (maybe-value-set? admin-token)
                        '("--env" "ADMIN_TOKEN")
                        '())
                  "--env" "DATABASE_URL"))))))))

(define vaultwarden-service-type
  (service-type
   (name 'vaultwarden)
   (extensions
    (list (service-extension account-service-type
                             (const %vaultwarden-accounts))
          (service-extension postgresql-role-service-type
                             vaultwarden-postgresql-role)
          (service-extension activation-service-type
                             vaultwarden-activation)
          (service-extension log-rotation-service-type
                             (compose list vaultwarden-configuration-log-file))
          (service-extension oci-service-type
                             vaultwarden-oci)))
   (description "Run Vaultwarden, a Bitwarden compatible server.")))
