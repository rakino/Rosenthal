;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services networking)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (rosenthal packages binaries)
  #:export (iwd-configuration
            iwd-service-type

            tailscale-configuration
            tailscale-service-type))

;;
;; iwd
;;


(define %iwd-config-general
  '(enable-network-configuration?
    use-default-interface?
    address-randomization
    address-randomization-range
    roam-threshold
    roam-threshold-5g
    roam-retry-interval
    management-frame-protection
    control-port-over-nl80211?
    disable-anqp?
    disable-ocv?
    country))

(define %iwd-config-network
  '(enable-ipv6?
    name-resolving-service
    route-priority-offset))

(define %iwd-config-blacklist
  '(initial-timeout
    multiplier
    maximum-timeout))

(define %iwd-config-rank
  '(band-modifier-5ghz
    band-modifier-6ghz))

(define %iwd-config-scan
  '(disable-periodic-scan?
    initial-periodic-scan-interval
    maximum-periodic-scan-interval
    disable-roaming-scan?))

(define %iwd-config-ipv4
  '(ap-address-pool))

(define %iwd-config-driver-quirks
  '(default-interface
    force-pae
    power-save-disable))

(define (uglify-field-name field-name)
  (case field-name
    ((control-port-over-nl80211?) "ControlPortOverNL80211")
    ((disable-anqp?) "DisableANQP")
    ((disable-ocv?) "DisableOCV")
    ((enable-ipv6?) "EnableIPv6")
    ((ap-address-pool) "APAddressPool")
    (else (string-delete char-set:punctuation
                         (string-capitalize (symbol->string field-name))))))

(define (serialize-field field-name val)
  (format #f "~a = ~a~%" (uglify-field-name field-name) val))

(define serialize-string serialize-field)

(define-maybe string)

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))

(define cidr4? (@@ (gnu services vpn) cidr4?))

(define serialize-cidr4 serialize-field)

(define-maybe cidr4)

(define (randomization-method? val)
  (memv val '(#f once network)))

(define (serialize-randomization-method field-name val)
  (serialize-field field-name (or val 'disabled)))

(define (randomization-range? val)
  (memv val '(full nic)))

(define serialize-randomization-range serialize-field)

(define (signal-strength? val)
  (and (number? val)
       (>= val -100)
       (<= val 1)))

(define serialize-signal-strength serialize-field)

(define (seconds? val)
  (and (integer? val)
       (not (negative? val))))

(define serialize-seconds serialize-field)

(define (protection-mode? val)
  (memv val '(0 1 2)))

(define serialize-protection-mode serialize-field)

(define (resolution-method? val)
  (memv val '(#f resolvconf)))

(define (serialize-resolution-method field-name val)
  (serialize-field field-name (or val 'none)))

(define serialize-integer serialize-field)

(define serialize-number serialize-field)

(define (serialize-list-of-strings field-name val)
  (serialize-field field-name (string-join val ",")))

(define-maybe list-of-strings)

(define list-of-cidr4? (list-of cidr4?))

(define serialize-list-of-cidr4 serialize-list-of-strings)

(define-configuration iwd-configuration
  (iwd
   (file-like iwd)
   "The iwd package to use.")

  (log-file
   (string "/var/log/iwd.log")
   "Log file location.")

  ;; General
  (enable-network-configuration?
   (boolean #f)
   "Enable network configuration.")

  (use-default-interface?
   (boolean #f)
   "Do not allow iwd to destroy / recreate wireless interfaces at startup,
including default interfaces.")

  (address-randomization
   (randomization-method #f)
   "Available values are @code{#f}, @code{once} and @code{network}.  @code{#f}
for default kernel behavior, @code{once} to randomize the MAC address when iwd
starts or the hardware is detected for the first time, @code{network} to
randomize the MAC address on each connection to a network (the MAC address is
generated based on the SSID and permanent address of the adapter).")

  (address-randomization-range
   (randomization-range 'full)
   "Available values are @code{nic} and @code{full}.  @code{nic} to only
randomize the NIC specific octets (last 3 ones), @code{full} to randomize all
6 octets of the address.")

  (roam-threshold
   (signal-strength -70)
   "Value in dBm, control how aggressively iwd roams when connected to a 2.4Ghz
access point.")

  (roam-threshold-5g
   (signal-strength -76)
   "Value in dBm, control how aggressively iwd roams when connected to a 5Ghz
access point.")

  (roam-retry-interval
   (seconds 60)
   "How long to wait before attempting to roam again if the last roam attempt
failed, or if the signal of the newly connected BSS is still considered weak.")

  (management-frame-protection
   (protection-mode 1)
   "Available values are @code{0}, @code{1} and @code{2}.  @code{0} to
completely turn off MFP (even if the hardware is capable), @code{1} to enable
MFP if the local hardware and remote AP both support it, @code{2} to always
require MFP.")

  (control-port-over-nl80211?
   (boolean #t)
   "Enable sending EAPoL packets over NL80211.")

  (disable-anqp?
   (boolean #t)
   "Disable ANQP queries.")

  (disable-ocv?
   (boolean #f)
   "Disable Operating Channel Validation.")

  (country
   maybe-string
   "ISO Alpha-2 Country Code.  Request the country to be set for the system.")

  ;; Network
  (enable-ipv6?
   (boolean #t)
   "Configure IPv6 addresses and routes.")

  (name-resolving-service
   (resolution-method 'resolvconf)
   "Available values are @code{resolvconf} and @code{#f}.  Configure a DNS
resolution method used by the system and must be used in conjunction with
@code{enable-network-configuration?}.  @code{#f} to ignore DNS and domain name
information.")

  (route-priority-offset
   (integer 300)
   "Configure a route priority offset used by the system to prioritize the
default routes.  The route with lower priority offset is preferred.")

  ;; Blacklist
  (initial-timeout
   (seconds 60)
   "The initial time that a BSS spends on the blacklist.")

  (multiplier
   (integer 30)
   "If the BSS was blacklisted previously and another connection attempt has
failed after the initial timeout has expired, then the BSS blacklist time will
be extended by a multiple of @code{multiplier} for each unsuccessful attempt up
to @code{maximum-timeout} time.")

  (maximum-timeout
   (seconds 86400)
   "Maximum time that a BSS is blacklisted.")

  ;; Rank
  (band-modifier-5ghz
   (number 1.0)
   "Increase or decrease the preference for 5GHz access points by increasing or
decreasing the value of this modifier.")

  (band-modifier-6ghz
   (number 1.0)
   "Increase or decrease the preference for 6GHz access points by increasing or
decreasing the value of this modifier.")

  ;; Scan
  (disable-periodic-scan?
   (boolean #f)
   "Disable periodic scan.")

  (initial-periodic-scan-interval
   (seconds 10)
   "The initial periodic scan interval upon disconnect.")

  (maximum-periodic-scan-interval
   (seconds 300)
   "The maximum periodic scan interval.")

  (disable-roaming-scan?
   (boolean #f)
   "Disable roaming scan.")

  ;; IPv4
  (ap-address-pool
   (list-of-cidr4 '("192.168.0.0/16"))
   "Define the space of IPs used for the AP mode subnet addresses and the DHCP
server.")

  ;; DriverQuirks
  (default-interface
   maybe-list-of-strings
   "List of drivers or glob matches.  If a driver in use matches one in this
list, IWD will not attempt to remove and re-create the default interface.")

  (force-pae
   maybe-list-of-strings
   "List of drivers or glob matches.  If a driver in use matches one in this
list, @code{control-port-over-nl80211?} will not be used, and PAE will be used
instead.")

  (power-save-disable
   maybe-list-of-strings
   "List of drivers or glob matches.  If a driver in use matches one in this
list, power save will be disabled."))

(define (serialize-iwd-configuration config)
  (apply mixed-text-file "main.conf"
         (append-map
          (match-lambda
            ((section . fields)
             (list "[" section "]\n"
                   (serialize-configuration
                    config
                    (filter-configuration-fields
                     iwd-configuration-fields
                     fields)))))
          `(("General"      . ,%iwd-config-general)
            ("Network"      . ,%iwd-config-network)
            ("Blacklist"    . ,%iwd-config-blacklist)
            ("Rank"         . ,%iwd-config-rank)
            ("Scan"         . ,%iwd-config-scan)
            ("IPv4"         . ,%iwd-config-ipv4)
            ("DriverQuirks" . ,%iwd-config-driver-quirks)))))

(define (add-iwd-config-file config)
  `(("iwd/main.conf"
     ,(serialize-iwd-configuration config))))

(define add-iwd-package
  (compose list iwd-configuration-iwd))

(define (iwd-log-rotations config)
  (list (log-rotation
         (files (list (iwd-configuration-log-file config))))))

(define (iwd-shepherd-service config)
  (match-record config <iwd-configuration>
                (iwd log-file
                     enable-network-configuration? name-resolving-service)
    (let ((conf (serialize-iwd-configuration config)))
      (list (shepherd-service
             (documentation "Run iwd")
             (provision `(,@(if enable-network-configuration?
                                '(networking)
                                '())
                          iwd))
             (requirement '(user-processes dbus-system))
             (start #~(make-forkexec-constructor
                       (list (string-append #$iwd "/libexec/iwd"))
                       #:log-file #$log-file))
             (stop #~(make-kill-destructor))
             (actions
              (list (shepherd-configuration-action "/etc/iwd/main.conf"))))))))

(define iwd-service-type
  (service-type
   (name 'iwd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             iwd-shepherd-service)
          (service-extension dbus-root-service-type
                             add-iwd-package)
          (service-extension etc-service-type
                             add-iwd-config-file)
          (service-extension profile-service-type
                             add-iwd-package)
          (service-extension rottlog-service-type
                             iwd-log-rotations)))
   (default-value (iwd-configuration))
   (description "Run iwd, the iNet wireless daemon.")))


;;
;; Tailscale
;;


(define-configuration tailscale-configuration
  (tailscale
   (file-like tailscale-bin)
   "The tailscale package to use.")

  (iptables
   (file-like iptables)
   "The iptables package to use.")

  (log-file
   (string "/var/log/tailscaled.log")
   "Path to log file.")

  (bird-socket
   maybe-string
   "Path of the bird UNIX socket.")

  (debug-server
   maybe-string
   "Listen address ([ip]:port) of optional debug server.")

  (port
   (integer 0)
   "UDP port to listen for WireGuard and peer-to-peer traffic; 0 means
automatically select.")

  (socket
   (string "/var/run/tailscale/tailscaled.sock")
   "Path of the service UNIX socket.")

  (http-proxy-server
   maybe-string
   "[ip]:port to run an outbound HTTP proxy (e.g. \"localhost:8080\").")

  (socks5-server
   maybe-string
   "[ip]:port to run a SOCKS5 server (e.g. \"localhost:1080\").")

  (state-directory
   (string "/var/lib/tailscale")
   "Path to directory for storage of config state, TLS certs, temporary incoming
Taildrop files, etc.  If empty, it's derived from @code{state-file} when
possible.")

  (state-file
   maybe-string
   "Absolute path of state file; use @code{kube:<secret-name>} to use Kubernetes
secrets or @code{arn:aws:ssm:...} to store in AWS SSM; use 'mem:' to not store
state and register as an ephemeral node.  If empty and @code{state-directory} is
provided, the default is @code{<state-directory>/tailscaled.state}.")

  (tunnel-interface
   (string "tailscale0")
   "Tunnel interface name; use @code{\"userspace-networking\"} (beta) to not use
TUN.")

  (upload-log?
   (boolean #f)
   "Whether to upload logs or not, technical support is also disabled when set
to #f.")

  (verbosity
   (integer 0)
   "Log verbosity level; 0 is default, 1 or higher are increasingly verbose.")
  (no-serialization))

(define (tailscale-log-rotations config)
  (list (log-rotation
         (files (list (tailscale-configuration-log-file config))))))

(define tailscale-shepherd-service
  (match-record-lambda <tailscale-configuration>
      (tailscale iptables log-file bird-socket debug-server port socket
                 http-proxy-server socks5-server state-directory state-file
                 tunnel-interface upload-log? verbosity)
    (let ((environment
           #~(list (string-append "PATH="
                                  (string-join
                                   '(#$(file-append iptables "/sbin")
                                     #$(file-append iproute "/sbin"))
                                   ":")))))
      (list (shepherd-service
             (documentation "Run tailscaled")
             (provision '(tailscaled))
             (requirement '(user-processes))
             (start
              #~(make-forkexec-constructor
                 (list
                  #$(file-append tailscale "/bin/tailscaled")
                  #$@(if (maybe-value-set? bird-socket)
                         `("-bird-socket" ,bird-socket)
                         '())
                  #$@(if (maybe-value-set? debug-server)
                         `("-debug" ,debug-server)
                         '())
                  #$@(if upload-log?
                         '()
                         '("-no-logs-no-support"))
                  #$@(if (maybe-value-set? http-proxy-server)
                         `("-outbound-http-proxy-listen" ,http-proxy-server)
                         '())
                  "-port" #$(number->string port)
                  "-socket" #$socket
                  #$@(if (maybe-value-set? socks5-server)
                         `("-socks5-server" ,socks5-server)
                         '())
                  #$@(if (maybe-value-set? state-file)
                         `("-state" ,state-file)
                         '())
                  "-statedir" #$state-directory
                  "-tun" #$tunnel-interface
                  "-verbose" #$(number->string verbosity))
                 #:environment-variables #$environment
                 #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define tailscale-service-type
  (service-type
   (name 'tailscaled)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-shepherd-service)
          (service-extension profile-service-type
                             (compose list tailscale-configuration-tailscale))
          (service-extension rottlog-service-type
                             tailscale-log-rotations)))
   (default-value (tailscale-configuration))
   (description "Run tailscaled.")))
