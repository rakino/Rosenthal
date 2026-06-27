;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services web)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (rosenthal utils file)
  #:use-module (rosenthal utils predicates)
  ;; Guix origin methods
  #:use-module (guix download)
  ;; Guix System
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu services databases)
  #:use-module (gnu services docker)
  #:use-module (gnu services shepherd)
  ;; Guix build systems
  #:use-module (guix build-system copy)
  ;; Guix packages
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages web)
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

            tuwunel-service-type
            tuwunel-configuration

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
  ;; User
  (group-id
   (user-and-group-id #f)
   "")
  (user-id
   (user-and-group-id #f)
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

(define caddy-accounts
  (match-record-lambda <caddy-configuration>
      (group-id user-id)
    (list (user-group
            (name "caddy")
            (id group-id)
            (system? #t))
          (user-account
            (name "caddy")
            (group "caddy")
            (uid user-id)
            (system? #t)
            (comment "Caddy user")
            (home-directory "/var/lib/caddy")))))

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
                   (shepherd-signal-action 'reload SIGUSR1)))
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

(define-maybe string)

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
   file-object-or-file-config
   "")
  (postgresql-password-file
   maybe-string
   "")
  (group-id
   (user-and-group-id #f)
   "")
  (user-id
   (user-and-group-id #f)
   "")
  (no-serialization))

(define forgejo-account
  (match-record-lambda <forgejo-configuration>
      (group-id user-id)
    (list (user-group
            (name "forgejo")
            (id group-id)
            (system? #t))
          (user-account
            (name "forgejo")
            (group "forgejo")
            (uid user-id)
            (system? #t)
            (comment "Forgejo user")
            (home-directory "/var/lib/forgejo")))))

(define forgejo-postgresql-role
  (match-record-lambda <forgejo-configuration>
      (postgresql-password-file)
    (list (postgresql-role
            (name "forgejo")
            (create-database? #t)
            (password-file
             (if (maybe-value-set? postgresql-password-file)
                 postgresql-password-file
                 #f))))))

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
           (if (file-config? config)
               (ini-file "forgejo.ini" config)
               config)))
      (list (shepherd-service
              (documentation "Run Forgejo.")
              (provision '(forgejo))
              (requirement '(loopback postgresql))
              (start
               #~(make-forkexec-constructor
                  (list "/run/current-system/profile/bin/forgejo"
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
                             forgejo-account)
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
   file-object-or-file-config
   "")
  (log-file
   (string "/var/log/iocaine.log")
   "")
  ;; Account
  (group-id
   (user-and-group-id #f)
   "")
  (user-id
   (user-and-group-id #f)
   "")
  ;; Shepherd
  (shepherd-provision
   (list-of-symbols '(iocaine))
   "")
  (shepherd-requirement
   (list-of-symbols '(loopback))
   "")
  (auto-start?
   (boolean #t)
   ""))

(define iocaine-account
  (match-record-lambda <iocaine-configuration>
      (group-id user-id)
    (list (user-group
            (name "iocaine")
            (id group-id)
            (system? #t))
          (user-account
            (name "iocaine")
            (group "iocaine")
            (uid user-id)
            (system? #t)
            (comment "Iocaine user")
            (home-directory "/var/empty")))))

(define iocaine-etc
  (match-record-lambda <iocaine-configuration>
      (config)
    `(("iocaine/iocaine.toml"
       ,(if (file-config? config)
            (toml-file "iocaine.toml" config)
            config)))))

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
                             iocaine-account)
          (service-extension etc-service-type
                             iocaine-etc)
          (service-extension shepherd-root-service-type
                             iocaine-shepherd-service)))
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
  (user-id
   (user-and-group-id #f)
   "")
  (auto-start?
   (boolean #t)
   "Whether to start automatically.")
  (extra-options
   (list '())
   "List of extra options.")
  (no-serialization))

(define jellyfin-account
  (match-record-lambda <jellyfin-configuration>
      (user-id)
    (list (user-account
            (name "jellyfin")
            (group "docker")
            (uid user-id)
            (system? #t)
            (home-directory "/var/empty")
            (shell (file-append shadow "/sbin/nologin"))))))

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
                             jellyfin-account)
          (service-extension activation-service-type
                             jellyfin-activation)
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
  (group-id
   (user-and-group-id #f)
   "")
  (user-id
   (user-and-group-id #f)
   "")
  (auto-start?
   (boolean #t)
   "Whether to start automatically.")
  (no-serialization))

(define komga-account
  (match-record-lambda <komga-configuration>
      (group-id user-id)
    (list (user-group
            (name "komga")
            (id group-id)
            (system? #t))
          (user-account
            (name "komga")
            (group "komga")
            (uid user-id)
            (system? #t)
            (comment "Komga user")
            (home-directory "/var/lib/komga")))))

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
                             komga-account)
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
   file-object-or-file-config
   "Alist of Misskey configuration, to be serialized to YAML format.")
  (data-directory
   (string "/var/lib/misskey")
   "Directory to store @file{files} in.")
  (user-id
   (user-and-group-id #f)
   "")
  (log-file
   (string "/var/log/misskey.log")
   "Log file to use.")
  (postgresql-password-file
   maybe-string
   "")
  (no-serialization))

(define misskey-account
  (match-record-lambda <misskey-configuration>
      (user-id)
    (list (user-account
            (name "misskey")
            (group "docker")
            (uid user-id)
            (system? #t)
            (home-directory "/var/empty")
            (shell (file-append shadow "/sbin/nologin"))))))

(define misskey-postgresql-role
  (match-record-lambda <misskey-configuration>
      (postgresql-password-file)
    (list (postgresql-role
            (name "misskey")
            (create-database? #t)
            (password-file
             (if (maybe-value-set? postgresql-password-file)
                 postgresql-password-file
                 #f))))))

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
           (if (file-config? config)
               (yaml-file "misskey.yaml" config)
               config)))
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
                             misskey-account)
          (service-extension postgresql-role-service-type
                             misskey-postgresql-role)
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
  (group-id
   (user-and-group-id #f)
   "")
  (user-id
   (user-and-group-id #f)
   "")
  (auto-start?
   (boolean #t)
   "")
  (config
   (file-object-or-file-config #~'())
   "")
  (no-serialization))

(define navidrome-account
  (match-record-lambda <navidrome-configuration>
      (group-id user-id)
    (list (user-group
            (name "navidrome")
            (id group-id)
            (system? #t))
          (user-account
            (name "navidrome")
            (group "navidrome")
            (uid user-id)
            (system? #t)
            (comment "Navidrome user")
            (home-directory "/var/lib/navidrome")))))

(define navidrome-shepherd-service
  (match-record-lambda <navidrome-configuration>
      (navidrome ffmpeg auto-start? config)
    (let ((config-file
           (if (file-config? config)
               (toml-file "navidrome.toml"
                 (let ((default-settings
                         '(("DataFolder" . "/var/lib/navidrome")
                           ("CacheFolder" . "/var/lib/navidrome/cache")
                           ("EnableInsightsCollector" . #f))))
                   (if (gexp? config)
                       #~(append #$default-settings #$config)
                       (append default-settings config))))
               config)))
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
                             navidrome-account)
          (service-extension shepherd-root-service-type
                             navidrome-shepherd-service)))
   (default-value (navidrome-configuration))
   (description "Run Navidrome.")))


;;;
;;; Tuwunel
;;;

(define-configuration/no-serialization tuwunel-configuration
  (tuwunel
   (file-like tuwunel-bin)
   "Tuwunel package to use.")
  (database-path
   (string "/var/lib/tuwunel")
   "Directory to create for @code{tuwunel} user.")
  (config
   file-object-or-file-config
   "Configuration file in @code{toml-file} format.")
  ;; Account
  (group-id
   (user-and-group-id #f)
   "Group id for @code{tuwunel} group.")
  (user-id
   (user-and-group-id #f)
   "User id for @code{tuwunel} user.")
  ;; Shepherd
  (auto-start?
   (boolean #t)
   "Whether or not to start the Shepherd service automatically.")
  (shepherd-requirement
   (list-of-symbols '())
   "List of Shepherd services that should be started before this service."))

(define tuwunel-account
  (match-record-lambda <tuwunel-configuration>
      (group-id user-id)
    (list (user-group
            (name "tuwunel")
            (id group-id)
            (system? #t))
          (user-account
            (name "tuwunel")
            (group "tuwunel")
            (uid user-id)
            (system? #t)
            (comment "Tuwunel user")
            (home-directory "/var/empty")
            (create-home-directory? #f)))))

(define tuwunel-activation
  (match-record-lambda <tuwunel-configuration>
      (database-path)
    (with-imported-modules (source-module-closure '((gnu build activation)))
      #~(begin
          (use-modules (gnu build activation))
          (mkdir-p/perms #$database-path (getpwnam "tuwunel") #o750)))))

(define tuwunel-shepherd
  (match-record-lambda <tuwunel-configuration>
      (tuwunel config auto-start? shepherd-requirement)
    (let ((config-file
           (if (file-config? config)
               (toml-file "tuwunel.toml" config)
               config)))
      (list (shepherd-service
              (provision '(tuwunel))
              (requirement `(networking user-processes ,@shepherd-requirement))
              (start
               #~(make-forkexec-constructor
                  (list #$(file-append tuwunel "/bin/tuwunel"))
                  #:user "tuwunel"
                  #:group "tuwunel"
                  #:log-file "/var/log/tuwunel.log"
                  #:environment-variables
                  (list (string-append "TUWUNEL_CONFIG=" #$config-file))))
              (stop #~(make-kill-destructor))
              (actions
               (list (shepherd-configuration-action config-file))))))))

(define tuwunel-service-type
  (service-type
    (name 'tuwunel)
    (extensions
     (list (service-extension account-service-type
                              tuwunel-account)
           (service-extension activation-service-type
                              tuwunel-activation)
           (service-extension shepherd-root-service-type
                              tuwunel-shepherd)))
    (description "Run Tuwunel.")))


;;;
;;; Vaultwarden
;;;

(define vaultwarden-web-vault
  (package
    (name "vaultwarden-web-vault")
    (version "2026.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dani-garcia/bw_web_builds/releases/download/v"
             version "/bw_web_v" version ".tar.gz"))
       (sha256
        (base32 "0hi6mdazzqcyvjkyr779kjisygwab85afgkn2sl0531x4a0js8xw"))))
    (build-system copy-build-system)
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define-configuration vaultwarden-configuration
  (vaultwarden
   (file-like (file-append vaultwarden "/bin/vaultwarden"))
   "")
  (web-vault
   (file-like vaultwarden-web-vault)
   "")
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
   maybe-string
   "")
  (group-id
   (user-and-group-id #f)
   "")
  (user-id
   (user-and-group-id #f)
   "")
  (no-serialization))

(define vaultwarden-account
  (match-record-lambda <vaultwarden-configuration>
      (group-id user-id data-directory)
    (list (user-group
            (name "vaultwarden")
            (id group-id)
            (system? #t))
          (user-account
            (name "vaultwarden")
            (group "vaultwarden")
            (uid user-id)
            (system? #t)
            (home-directory data-directory)
            (shell (file-append shadow "/sbin/nologin"))))))

(define vaultwarden-postgresql-role
  (match-record-lambda <vaultwarden-configuration>
      (postgresql-password-file)
    (list (postgresql-role
            (name "vaultwarden")
            (create-database? #t)
            (password-file
             (if (maybe-value-set? postgresql-password-file)
                 postgresql-password-file
                 #f))))))

(define vaultwarden-activation
  (match-record-lambda <vaultwarden-configuration>
      (data-directory)
    (with-imported-modules '((gnu build activation))
      #~(begin
          (use-modules (gnu build activation))
          (mkdir-p/perms #$data-directory (getpwnam "vaultwarden") #o700)))))

(define vaultwarden-shepherd-extension
  (match-record-lambda <vaultwarden-configuration>
      (vaultwarden web-vault admin-token database-url port data-directory log-file proxy-url extra-options)
    (list (shepherd-service
            (provision '(vaultwarden))
            (requirement '(user-processes loopback postgresql))
            (modules '((ice-9 match)))
            (start
             #~(make-forkexec-constructor
                (list #$vaultwarden)
                #:group "vaultwarden"
                #:user "vaultwarden"
                #:log-file #$log-file
                #:environment-variables
                (map (match-lambda
                       ((variable . value)
                        (string-append variable "=" value)))
                     `(#$@(if (maybe-value-set? admin-token)
                              `(("ADMIN_TOKEN" . ,admin-token))
                              '())
                       #$@(if (maybe-value-set? proxy-url)
                              `(("HTTP_PROXY" . ,proxy-url))
                              '())
                       ("DATA_FOLDER" . #$data-directory)
                       ("WEB_VAULT_FOLDER" . #$web-vault)
                       ("DATABASE_URL" . #$database-url)
                       ("ROCKET_PORT" . #$(number->string port))
                       ("USE_SYSLOG" . "True")
                       #$@extra-options))))
            (stop #~(make-kill-destructor))))))

(define vaultwarden-service-type
  (service-type
   (name 'vaultwarden)
   (extensions
    (list (service-extension account-service-type
                             vaultwarden-account)
          (service-extension postgresql-role-service-type
                             vaultwarden-postgresql-role)
          (service-extension activation-service-type
                             vaultwarden-activation)
          (service-extension shepherd-root-service-type
                             vaultwarden-shepherd-extension)))
   (description "Run Vaultwarden, a Bitwarden compatible server.")))
