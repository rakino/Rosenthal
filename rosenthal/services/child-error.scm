;; SPDX-FileCopyrightText: 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services child-error)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages networking)
  #:use-module (rosenthal utils home-services-utils)
  #:export (clash-configuration
            clash-service-type

            cloudflare-tunnel-configuration
            cloudflare-tunnel-service-type

            miniflux-configuration
            miniflux-service-type

            home-wakapi-configuration
            home-wakapi-service-type))

;; Child-error: services for packages not available in Guix, currently this
;; means some Go and Rust apps I build locally but don't want to package.


;;
;; Clash
;;


(define-configuration/no-serialization clash-configuration
  (clash
   (file-like clash-bin)
   "The clash package.")
  (log-file
   (string "/var/log/clash.log")
   "Where the logs go.")
  (data-directory
   (string "/var/lib/clash")
   "Where to store data.")
  (config
   (file-like (plain-file "empty" ""))
   "Clash configuration file."))

(define %clash-accounts
  (list (user-group (name "clash") (system? #t))
        (user-account
         (name "clash")
         (group "clash")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define clash-activation
  (match-lambda
    (($ <clash-configuration> clash log-file data-directory config)
     #~(begin
         (use-modules (guix build utils))
         (let ((config-dest (string-append #$data-directory "/config.yaml"))
               (user (getpwnam "clash")))
           (mkdir-p #$data-directory)
           (chown #$data-directory (passwd:uid user) (passwd:gid user))
           (if (file-exists? config-dest)
               (delete-file config-dest))
           (symlink #$config config-dest))))))

(define clash-shepherd-service
  (match-lambda
    (($ <clash-configuration> clash log-file data-directory config)
     (list (shepherd-service
            (documentation "Run clash.")
            (provision '(clash))
            (requirement '(loopback networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append clash "/bin/clash")
                            "-d" #$data-directory)
                      #:user "clash"
                      #:group "clash"
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor)))))))

(define clash-service-type
  (service-type
   (name 'clash)
   (extensions
    (list (service-extension shepherd-root-service-type
                             clash-shepherd-service)
          (service-extension activation-service-type
                             clash-activation)
          (service-extension account-service-type
                             (const %clash-accounts))))
   (default-value (clash-configuration))
   (description "Run Clash.")))



;;
;; Cloudflare Tunnel
;;


(define-configuration/no-serialization cloudflare-tunnel-configuration
  (cloudflared
   (package cloudflared)
   "The cloudflared executable.")

  ;; Tunnel options
  (metrics
   (string "localhost:")
   "Listen address for metrics reporting.")
  (log-level
   (string "info")
   "Application logging level (@code{debug}, @code{info}, @code{warn},
@code{error}, @code{fatal}).  At debug level cloudflared will log request URL,
method, protocol, content length, as well as, all request and response
headers.  This can expose sensitive information in your logs.")
  (log-file
   (string "/var/log/cloudflared.log")
   "File path to store logs.")
  (extra-tunnel-options
   (list-of-strings '())
   "List of extra tunnel options.")

  ;; Subcommand options
  (token
   (string "")
   "The Tunnel token.")
  (http2-origin?
   (boolean #f)
   "Enable HTTP/2 origin servers.")
  (post-quantum?
   (boolean #f)
   "Create an experimental post-quantum secure tunnel.")
  (extra-options
   (list-of-strings '())
   "List of extra options."))

(define cloudflare-tunnel-shepherd-service
  (match-lambda
    (($ <cloudflare-tunnel-configuration> cloudflared metrics
                                          log-level log-file
                                          extra-tunnel-options
                                          token http2-origin? post-quantum?
                                          extra-options)
     (list (shepherd-service
            (documentation "Run cloudflared.")
            (provision '(cloudflare-tunnel))
            (requirement '(loopback networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append cloudflared "/bin/cloudflared")
                            "tunnel"
                            "--no-autoupdate"
                            "--metrics" #$metrics
                            "--loglevel" #$log-level
                            #$@extra-tunnel-options

                            "run"
                            "--token" #$token
                            #$@(if http2-origin?
                                   '("--http2-origin")
                                   '())
                            #$@(if post-quantum?
                                   '("--post-quantum")
                                   '())
                            #$@extra-options)
                      #:user "nobody"
                      #:group "nogroup"
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor)))))))

(define cloudflare-tunnel-service-type
  (service-type
   (name 'cloudflare-tunnel)
   (extensions
    (list (service-extension shepherd-root-service-type
                             cloudflare-tunnel-shepherd-service)))
   (default-value (cloudflare-tunnel-configuration))
   (description "Run cloudflared, the Cloudflare Tunnel daemon.")))


;;
;; Miniflux
;;

(define-configuration/no-serialization miniflux-configuration
  (miniflux
   (package miniflux)
   "The miniflux package.")
  (log-file
   (string "/var/log/miniflux.log")
   "Where the logs go.")
  (options
   (alist '())
   "Association list of miniflux configuration options."))

(define %miniflux-accounts
  (list (user-account
         (name "miniflux")
         (group "nogroup")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %miniflux-postgresql-role
  (list (postgresql-role
         (name "miniflux")
         (create-database? #t))))

(define (miniflux-shepherd-service config)
  (match-record config <miniflux-configuration>
    (miniflux log-file options)
    (let ((config-file (mixed-text-file
                        "miniflux.conf"
                        (apply string-append
                               (map (lambda (option)
                                      (format #f "~a=~a~%"
                                              (car option) (cdr option)))
                                    options)))))
      (list (shepherd-service
             (documentation "Run miniflux.")
             (provision '(miniflux))
             (requirement '(postgres user-processes))
             (start #~(make-forkexec-constructor
                       (list #$miniflux "-config-file" #$config-file)
                       #:user "miniflux"
                       #:group "nogroup"
                       #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define miniflux-service-type
  (service-type
   (name 'miniflux)
   (extensions
    (list (service-extension account-service-type
                             (const %miniflux-accounts))
          (service-extension postgresql-role-service-type
                             (const %miniflux-postgresql-role))
          (service-extension shepherd-root-service-type
                             miniflux-shepherd-service)))
   (default-value (miniflux-configuration))
   (description "Run Miniflux, a minimalist and opinionated feed reader.")))


;;
;; Wakapi
;;


(define-configuration/no-serialization home-wakapi-configuration
  (wakapi
   (string "/bin/wakapi")
   "The wakapi executable.")
  (config
   (yaml-config '())
   "Association list of Wakapi configurations."))

(define home-wakapi-shepherd-service
  (match-lambda
    (($ <home-wakapi-configuration> wakapi config)
     (let ((config-file (mixed-text-file
                         "wakapi.yaml"
                         #~(string-append #$@(serialize-yaml-config config) "\n"))))
       (list (shepherd-service
              (documentation "Run wakapi.")
              (provision '(wakapi))
              (start #~(make-forkexec-constructor
                        (list #$wakapi "-config" #$config-file)))))))))

(define home-wakapi-service-type
  (service-type
   (name 'home-wakapi)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-wakapi-shepherd-service)))
   (default-value (home-wakapi-configuration))
   (description "Run Wakapi, a self-hosted WakaTime-compatible backend.")))
