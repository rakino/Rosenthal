;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services child-error)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal utils home-services-utils)
  #:export (clash-configuration
            clash-service-type

            cloudflare-tunnel-configuration
            cloudflare-tunnel-service-type))

;; Child-error: services for packages not available in Guix, currently this
;; means some Go and Rust apps I build locally but don't want to package.


;;
;; Clash
;;


(define-configuration/no-serialization clash-configuration
  (clash
   (string "/bin/clash")
   "The clash executable.")
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
                      (list #$clash "-d" #$data-directory)
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
   (string "/bin/cloudflared")
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

(define %cloudflare-tunnel-accounts
  (list (user-group (name "cloudflared") (system? #t))
        (user-account
         (name "cloudflared")
         (group "cloudflared")
         (system? #t)
         (comment "Cloudflare Tunnel user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

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
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$cloudflared
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
                      #:user "cloudflared"
                      #:group "cloudflared"
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor)))))))

(define cloudflare-tunnel-service-type
  (service-type
   (name 'cloudflare-tunnel)
   (extensions
    (list (service-extension shepherd-root-service-type
                             cloudflare-tunnel-shepherd-service)
          (service-extension account-service-type
                             (const %cloudflare-tunnel-accounts))))
   (default-value (cloudflare-tunnel-configuration))
   (description "Run cloudflared, the Cloudflare Tunnel daemon.")))
