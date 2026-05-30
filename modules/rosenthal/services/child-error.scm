;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2023 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal services child-error)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rosenthal utils file)
  #:use-module (rosenthal utils predicates)
  ;; Guix System
  #:use-module (gnu system shadow)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  ;; Guix Home - services
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  ;; Guix packages
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (rosenthal packages binaries)
  #:use-module (rosenthal packages networking)
  #:export (clash-configuration
            clash-service-type

            cloudflare-tunnel-configuration
            cloudflare-tunnel-service-type

            cloudflare-warp-configuration
            cloudflare-warp-service-type

            home-wakapi-configuration
            home-wakapi-service-type

            shadow-tls-configuration
            shadow-tls-client-configuration
            shadow-tls-server-configuration
            shadow-tls-service-type
            home-shadow-tls-service-type

            home-socks2http-configuration
            home-socks2http-service-type))

;;;
;;; Clash
;;;


(define-configuration clash-configuration
  (clash
   (file-like mihomo)
   "The clash package.")

  (log-file
   (string "/var/log/clash.log")
   "Where the logs go.")

  (data-directory
   (string "/var/lib/clash")
   "Where to store data.")

  (config
   (file-like (plain-file "empty" ""))
   "Clash configuration file.")
  ;; Account
  (group-id
   (user-and-group-id #f)
   "")
  ;; Shepherd
  (shepherd-provision
   (list '(clash))
   "A list of Shepherd service names (symbols) provided by this service.")
  (no-serialization))

(define clash-account
  (match-record-lambda <clash-configuration>
      (group-id)
    (list (user-group
            (name "clash")
            (id group-id)
            (system? #t)))))

(define clash-activation
  (match-record-lambda <clash-configuration>
      (data-directory config)
    #~(begin
        (use-modules (guix build utils))
        (let ((config-dest (string-append #$data-directory "/config.yaml")))
          (mkdir-p #$data-directory)
          (if (file-exists? config-dest)
              (delete-file config-dest))
          (symlink #$config config-dest)))))

(define clash-shepherd-service
  (match-record-lambda <clash-configuration>
      (clash log-file data-directory shepherd-provision)
    (list (shepherd-service
           (documentation "Run clash.")
           (provision shepherd-provision)
           (requirement '(loopback networking))
           (start #~(make-forkexec-constructor
                     (list (let ((mihomo-cmd
                                  #$(file-append clash "/bin/mihomo"))
                                 (clash-cmd
                                  #$(file-append clash "/bin/clash")))
                             (if (file-exists? mihomo-cmd)
                                 mihomo-cmd
                                 clash-cmd))
                           "-d" #$data-directory)
                     #:group "clash"
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor))
           (actions
            (list (shepherd-configuration-action
                   (string-append data-directory "/config.yaml"))))))))

(define clash-service-type
  (service-type
   (name 'clash)
   (extensions
    (list (service-extension shepherd-root-service-type
                             clash-shepherd-service)
          (service-extension activation-service-type
                             clash-activation)
          (service-extension account-service-type
                             clash-account)))
   (default-value (clash-configuration))
   (description "Run Clash.")))


;;;
;;; Cloudflare Tunnel
;;;


(define-maybe string)

(define-configuration cloudflare-tunnel-configuration
  (cloudflared
   (file-like cloudflared)
   "The cloudflared executable.")

  ;; Tunnel options
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
   maybe-string
   "The Tunnel token.")
  (token-file
   maybe-string
   "Secert file for the Tunnel token.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  ;; Account
  (user-id
   (user-and-group-id #f)
   "")
  (no-serialization))

(define cloudflare-tunnel-account
  (match-record-lambda <cloudflare-tunnel-configuration>
      (user-id)
    (list (user-account
            (name "cloudflared")
            (group "nogroup")
            (uid user-id)
            (system? #t)
            (home-directory "/var/empty")
            (create-home-directory? #f)
            (shell (file-append shadow "/sbin/nologin"))))))

(define cloudflare-tunnel-shepherd-service
  (match-record-lambda <cloudflare-tunnel-configuration>
      (cloudflared log-level log-file extra-tunnel-options
                   token token-file extra-options)
    (list (shepherd-service
           (documentation "Run cloudflared.")
           (provision '(cloudflare-tunnel cloudflared))
           (requirement '(loopback networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append cloudflared "/bin/cloudflared")
                           "tunnel"
                           "--no-autoupdate"
                           "--loglevel" #$log-level
                           #$@extra-tunnel-options
                           "run"
                           #$@extra-options)
                     #:user "cloudflared"
                     #:group "nogroup"
                     #:log-file #$log-file
                     #:environment-variables
                     (list #$@(if (maybe-value-set? token)
                                  (list (format #f "TUNNEL_TOKEN=~a"
                                                token))
                                  '())
                           #$@(if (maybe-value-set? token-file)
                                  (list (format #f "TUNNEL_TOKEN_FILE=~a"
                                                token-file))
                                  '()))))
           (stop #~(make-kill-destructor))))))

(define cloudflare-tunnel-service-type
  (service-type
   (name 'cloudflare-tunnel)
   (extensions
    (list (service-extension shepherd-root-service-type
                             cloudflare-tunnel-shepherd-service)
          (service-extension account-service-type
                             cloudflare-tunnel-account)))
   (default-value (cloudflare-tunnel-configuration))
   (description "Run cloudflared, the Cloudflare Tunnel daemon.")))


;;;
;;; Cloudflare Warp
;;;


(define-configuration cloudflare-warp-configuration
  (cloudflare-warp
   (file-like cloudflare-warp-bin)
   "The Cloudflare Warp package.")
  (no-serialization))

(define cloudflare-warp-shepherd-service
  (match-record-lambda  <cloudflare-warp-configuration>
      (cloudflare-warp)
    (list (shepherd-service
           (documentation "Run warp-svc.")
           (provision '(cloudflare-warp))
           (start #~(make-forkexec-constructor
                     (list #$(file-append cloudflare-warp "/bin/warp-svc"))
                     ;; Logs are written to
                     ;; /var/lib/cloudflare-warp/cfwarp_service_log.txt.
                     #:log-file "/dev/null"))
           (stop #~(make-kill-destructor))))))

(define cloudflare-warp-service-type
  (service-type
   (name 'cloudflare-warp)
   (extensions
    (list (service-extension shepherd-root-service-type
                             cloudflare-warp-shepherd-service)
          (service-extension
           profile-service-type
           (compose list cloudflare-warp-configuration-cloudflare-warp))))
   (default-value (cloudflare-warp-configuration))
   (description "Run warp-svc, the Cloudflare Warp daemon.")))


;;;
;;; Wakapi
;;;


(define-configuration home-wakapi-configuration
  (wakapi
   (file-like wakapi-bin)
   "The wakapi package.")
  (config
   file-object-or-file-config
   "Association list of Wakapi configurations.")
  (no-serialization))

(define home-wakapi-shepherd-service
  (match-record-lambda <home-wakapi-configuration>
      (wakapi config)
    (let ((config-file
           (if (file-config? config)
               (yaml-file "wakapi.yaml" config)
               config)))
      (list (shepherd-service
             (documentation "Run wakapi.")
             (provision '(wakapi))
             (start #~(make-forkexec-constructor
                       (list #$(file-append wakapi "/bin/wakapi")
                             "-config" #$config-file)))
             (stop #~(make-kill-destructor))
             (actions (list (shepherd-configuration-action config-file))))))))

(define home-wakapi-service-type
  (service-type
   (name 'home-wakapi)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-wakapi-shepherd-service)))
   (description "Run Wakapi, a self-hosted WakaTime-compatible backend.")))


;;;
;;; ShadowTLS
;;;


(define-maybe list-of-strings
  (no-serialization))

(define-configuration shadow-tls-client-configuration
  (listen-address
   (string "")
   "Listen address with port.  Usually this port is used by Shadowsocks
client.")
  (server-address
   (string "")
   "ShadowTLS server address with port.")
  (sni-list
   (list-of-strings '(""))
   "SNI list.")
  (password
   (string "")
   "Must be the same as the ShadowTLS server.")
  (alpn
   maybe-list-of-strings
   "ALPN ext.  Do not use unless you know what you are doing.")
  (no-serialization))

(define-configuration shadow-tls-server-configuration
  (listen-address
   (string "")
   "Listen address with port.")
  (server-address
   (string "")
   "Data server address with port.  Usually this port is listened by Shadowsocks
server.")
  (tls-name-list
   (list-of-strings '(""))
   "TLS names.  There must be a fallback server name at the last, and there can
be multiple mappings.  Mappings can be represented as
@code{ServerName:Host:Port}.  Host can be omitted, in this case
@code{ServerName} is used as @code{Host}.  @code{Port} can be omitted too, which
is @code{443} by default")
  (password
   (string "")
   "Must be the same as the ShadowTLS client.")
  (no-serialization))

(define-record-type* <shadow-tls-configuration> shadow-tls-configuration
  make-shadow-tls-configuration
  shadow-tls-configuration?
  this-shadow-tls-configuration

  (shadow-tls    shadow-tls-configuration-shadow-tls        ;file-like
                 (default shadow-tls-bin))
  (threads       shadow-tls-configuration-threads           ;integer | #f
                 (default #f))
  (no-delay?     shadow-tls-configuration-disable-no-delay? ;boolean
                 (default #t))
  (v3-protocol?  shadow-tls-configuration-v3-protocol?      ;boolean
                 (default #f))
  (log-level     shadow-tls-configuration-log-level         ;string
                 (default "info"))
  (client        shadow-tls-configuration-client ;<shadow-tls-client-configuration> | #f
                 (default #f))
  (server        shadow-tls-configuration-server ;<shadow-tls-server-configuration> | #f
                 (default #f))
  (home-service? shadow-tls-configuration-home-service?
                 (default for-home?) (innate)))

(define shadow-tls-shepherd-service
  (match-record-lambda <shadow-tls-configuration>
      (shadow-tls threads no-delay? v3-protocol? log-level client server
                  home-service?)
    (let ((common-options
           (append (if threads
                       `("--threads" ,(number->string threads))
                       '())
                   (if no-delay?
                       '()
                       '("--disable-nodelay"))
                   (if v3-protocol?
                       '("--v3")
                       '()))))
      (append
       (if client
          (match-record client
               <shadow-tls-client-configuration>
               (listen-address server-address sni-list password alpn)
             (let ((log-file
                    (if home-service?
                        #~(string-append %user-log-dir "/shadow-tls-client.log")
                        "/var/log/shadow-tls-client.log")))
               (list (shepherd-service
                      (documentation "Run shadow-tls client.")
                      (provision '(shadow-tls-client))
                      (requirement (if home-service? '() '(networking)))
                      (modules '((shepherd support)))
                      (start #~(make-forkexec-constructor
                                (list #$(file-append
                                         shadow-tls "/bin/shadow-tls")
                                      #$@common-options
                                      "client"
                                      "--listen" #$listen-address
                                      "--server" #$server-address
                                      "--sni" #$(string-join sni-list ";")
                                      "--password" #$password
                                      #$@(if (maybe-value-set? alpn)
                                             `("--alpn" ,(string-join alpn ";"))
                                             '()))
                                #:user #$(and (not home-service?) "nobody")
                                #:group #$(and (not home-service?) "nogroup")
                                #:log-file #$log-file
                                #:environment-variables
                                (list (string-append "RUST_LOG=" #$log-level))))
                      (stop #~(make-kill-destructor))))))
           '())
       (if server
           (match-record server
               <shadow-tls-server-configuration>
               (listen-address server-address tls-name-list password)
             (let ((log-file
                    (if home-service?
                        #~(string-append %user-log-dir "/shadow-tls-server.log")
                        "/var/log/shadow-tls-server.log")))
               (list (shepherd-service
                      (documentation "Run shadow-tls server.")
                      (provision '(shadow-tls-server))
                      (requirement (if home-service? '() '(networking)))
                      (modules '((shepherd support)))
                      (start #~(make-forkexec-constructor
                                (list #$(file-append
                                         shadow-tls "/bin/shadow-tls")
                                      #$@common-options
                                      "server"
                                      "--listen" #$listen-address
                                      "--server" #$server-address
                                      "--tls" #$(string-join tls-name-list ";")
                                      "--password" #$password)
                                #:user #$(and (not home-service?) "nobody")
                                #:group #$(and (not home-service?) "nogroup")
                                #:log-file #$log-file))
                      (stop #~(make-kill-destructor))))))
           '())))))

(define shadow-tls-service-type
  (service-type
   (name 'shadow-tls)
   (extensions
    (list (service-extension shepherd-root-service-type
                             shadow-tls-shepherd-service)))
   (default-value (shadow-tls-server-configuration))
   (description "Run shadow-tls.")))

(define home-shadow-tls-service-type
  (service-type
   (inherit (system->home-service-type shadow-tls-service-type))
   (default-value (for-home (shadow-tls-configuration)))))


;;;
;;; Socks2http
;;;


(define-configuration home-socks2http-configuration
  (socks2http
   (file-like socks2http)
   "Socks2http package to use.")
  (socks-address
   (string ":1080")
   "SOCKS5 proxy address to connect to.")
  (http-address
   (string ":8000")
   "HTTP proxy address to serve.")
  (no-serialization))

(define home-socks2http-shepherd-service
  (match-record-lambda <home-socks2http-configuration>
      (socks2http socks-address http-address)
    (list (shepherd-service
           (documentation "Run socks2http.")
           (provision '(socks2http))
           (start #~(make-forkexec-constructor
                     (list #$(file-append socks2http "/bin/socks2http")
                           "-raddr" #$socks-address
                           "-laddr" #$http-address)))
           (stop #~(make-kill-destructor))))))

(define home-socks2http-service-type
  (service-type
   (name 'home-socks2http)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-socks2http-shepherd-service)))
   (default-value (home-socks2http-configuration))
   (description "Run socks2http.")))




(define-service-type-mapping
  shadow-tls-service-type => home-shadow-tls-service-type)
