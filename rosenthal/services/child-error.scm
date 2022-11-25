;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services child-error)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (rosenthal utils home-services-utils)
  #:export (cloudflare-tunnel-configuration
            cloudflare-tunnel-service-type))

;; Child-error: services for packages not available in Guix, currently this
;; means some Go and Rust apps I build locally but don't want to package.



;;
;; Cloudflare Tunnel
;;


(define-record-type* <cloudflare-tunnel-configuration>
  cloudflare-tunnel-configuration make-cloudflare-tunnel-configuration
  cloudflare-tunnel-configuration?
  (cloudflared   cloudflare-tunnel-configuration-cloudflared   ;string
                 (default "/bin/cloudflared"))
  ;; Tunnel command options
  (metrics       cloudflare-tunnel-configuration-metrics       ;string
                 (default "localhost:"))
  (log-level     cloudflare-tunnel-configuration-log-level     ;string
                 (default "info"))
  (log-file      cloudflare-tunnel-configuration-log-file      ;string
                 (default "/var/log/cloudflared.log"))
  ;; Subcommand options
  (token         cloudflare-tunnel-configuration-token         ;string
                 (default #f))
  (http2-origin? cloudflare-tunnel-configuration-http2-origin? ;boolean
                 (default #f))
  (post-quantum? cloudflare-tunnel-configuration-post-quantum? ;boolean
                 (default #f))
  (extra-options cloudflare-tunnel-configuration-extra-options ;list of string
                 (default '())))

(define cloudflare-tunnel-shepherd-service
  (match-lambda
    (($ <cloudflare-tunnel-configuration> cloudflared metrics
                                          log-level log-file
                                          token http2-origin? post-quantum?
                                          extra-options)
     (let ((tunnel-options
            (list "--no-autoupdate" "--metrics" metrics "--loglevel" log-level)))
       (list (shepherd-service
              (documentation "Run cloudflared.")
              (provision '(cloudflare-tunnel))
              (requirement '(loopback networking user-processes))
              (start #~(make-forkexec-constructor
                        (list #$cloudflared "tunnel" #$@tunnel-options "run"
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
              (stop #~(make-kill-destructor))))))))

(define cloudflare-tunnel-service-type
  (service-type
   (name 'cloudflare-tunnel)
   (extensions
    (list (service-extension shepherd-root-service-type
                             cloudflare-tunnel-shepherd-service)))
   (default-value (cloudflare-tunnel-configuration))
   (description "Run cloudflared, the Cloudflare Tunnel daemon.")))
