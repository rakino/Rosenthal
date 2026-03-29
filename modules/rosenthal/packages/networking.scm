;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022-2024 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages networking)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (rosenthal utils packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  ;; Guix build systems
  #:use-module (guix build-system go)
  ;; Guix packages
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages linux))

(define-public cloudflared
  (package
    (name "cloudflared")
    (version "2026.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cloudflare/cloudflared")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; TODO: Unbundle vendored dependencies.
              ;; (modules '((guix build utils)))
              ;; (snippet '(delete-file-recursively "vendor"))
              (sha256
               (base32
                "17cvbkxrs1768r48iczxq75kr698cys0dymdbbhs9p1c75m0b0si"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.24
           #:install-source? #f
           #:import-path "github.com/cloudflare/cloudflared/cmd/cloudflared"
           #:unpack-path "github.com/cloudflare/cloudflared"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X main.Version=" #$(package-version this-package)
                    " -X github.com/cloudflare/cloudflared/cmd/cloudflared/updater.BuiltForPackageManager=Guix"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'disable-cgo
                 (lambda _
                   (setenv "CGO_ENABLED" "0")))
               (add-after 'install 'install-documentation
                 (lambda _
                   (let ((src "src/github.com/cloudflare/cloudflared/cloudflared_man_template")
                         (dst (string-append #$output "/share/man/man1/cloudflared.1")))
                     (substitute* src
                       (("\\$\\{VERSION\\}") #$(package-version this-package)))
                     (mkdir-p (dirname dst))
                     (copy-file src dst)))))))
    (home-page "https://developers.cloudflare.com/cloudflare-one/connections/connect-apps/")
    (synopsis "Cloudflare Tunnel client")
    (description
     "This package provides the command-line client for Cloudflare Tunnel, a
tunneling daemon that proxies traffic from the Cloudflare network to your
origins.  This daemon sits between Cloudflare network and your origin (e.g. a
webserver).  Cloudflare attracts client requests and sends them to you via
this daemon, without requiring you to poke holes on your firewall --- your
origin can remain as closed as possible.")
    (license license:asl2.0)))

(define-public mihomo
  (package
    (name "mihomo")
    (version "1.19.21")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MetaCubeX/mihomo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qa6fw3axnas39ri0qmy54rvyk3xbpnmv3iss21wrhjvnlcagmdw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? (not (%current-target-system)) ;TODO: Run test suite.
      #:go go-1.26
      #:install-source? #f
      #:import-path "."
      #:build-flags
      #~(list "-tags" "with_gvisor"
              (string-append
               "-ldflags="
               " -X github.com/metacubex/mihomo/constant.Version="
               #$(package-version this-package)))
      #:modules
      '((ice-9 match)
        ((guix build gnu-build-system) #:prefix gnu:)
        (guix build go-build-system)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda args
              (unsetenv "GO111MODULE")
              (apply (assoc-ref gnu:%standard-phases 'unpack) args)
              (copy-recursively
               #+(this-package-native-input "vendored-go-dependencies")
               "vendor")))
          (replace 'install-license-files
            (assoc-ref gnu:%standard-phases 'install-license-files))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (let ((mihomo (in-vicinity #$output "bin/mihomo")))
                  (invoke mihomo "--help")
                  (invoke mihomo "-v"))))))))
    (native-inputs
     (append
      (list (origin
              (method (go-mod-vendor #:go go-1.26))
              (uri (package-source this-package))
              (file-name "vendored-go-dependencies")
              (sha256
               (base32
                "15452bapz7jwi9gawww9kyszbj2pfdy50xg5zcs2rjrjkh5aygya"))))
      (if (%current-target-system)
          (list this-package)
          '())))
    (home-page "https://wiki.metacubex.one/")
    (synopsis "Rule-based proxy")
    (description
     "Mihomo is an anti-censorship proxy application, originally known as
\"Clash Meta\", designed to facilitate secure and flexible internet access.
It supports various protocols, making it a versatile tool for users seeking to
bypass network restrictions." )
    (license license:gpl3+)))

(define-public sing-box
  (package
    (name "sing-box")
    (version "1.12.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SagerNet/sing-box")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wr97myxdskqlh1mrpn0rnnvdkifaqnm420j3y6hbk9rf4ijzf3l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? (not (%current-target-system)) ;TODO: Run test suite.
      #:go go-1.23
      #:install-source? #f
      #:import-path "./cmd/sing-box"
      #:build-flags
      #~(list "-tags" (string-join
                       '("with_quic"
                         "with_dhcp"
                         "with_wireguard"
                         "with_tailscale"
                         "with_utls"
                         "with_acme"
                         "with_clash_api"
                         "with_gvisor"))
              (string-append
               "-ldflags="
               " -X github.com/sagernet/sing-box/constant.Version="
               #$(package-version this-package)))
      #:modules
      '((ice-9 match)
        ((guix build gnu-build-system) #:prefix gnu:)
        (guix build go-build-system)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda args
              (unsetenv "GO111MODULE")
              (apply (assoc-ref gnu:%standard-phases 'unpack) args)
              (copy-recursively
               #+(this-package-native-input "vendored-go-dependencies")
               "vendor")))
          (replace 'install-license-files
            (assoc-ref gnu:%standard-phases 'install-license-files))
          (add-after 'unpack 'set-tailscale-default-wireguard-port
            (lambda _
              ;; See also: https://tailscale.com/kb/1082/firewall-ports
              ;; https://github.com/tailscale/tailscale/blob/51c11a864b1241d1cf1a736fbc94b0f8c76da563/cmd/tailscaled/tailscaled.go#L102
              (substitute* "vendor/github.com/sagernet/tailscale/tsnet/tsnet.go"
                (("s\\.Port") "41641"))))
          (add-after 'install 'install-extras
            (lambda _
              (let ((sing-box
                     (or (which "sing-box")
                         (in-vicinity #$output "bin/sing-box"))))
                (map
                 (match-lambda
                   ((shell . path)
                    (let ((file (in-vicinity #$output path)))
                      (mkdir-p (dirname file))
                      (with-output-to-file file
                        (lambda ()
                          (invoke sing-box "completion" shell))))))
                 '(("bash" . "etc/bash_completion.d/sing-box")
                   ("fish" . "share/fish/vendor_completions.d/sing-box.fish")
                   ("zsh"  . "share/zsh/site-functions/_sing-box")))))))))
    (native-inputs
     (append
      (list (origin
              (method (go-mod-vendor #:go go-1.23))
              (uri (package-source this-package))
              (file-name "vendored-go-dependencies")
              (sha256
               (base32
                "0h3m4rfkwdcm22f8vbdl3idki46nxfmynagvy7s00lycylz1f809"))))
      (if (%current-target-system)
          (list this-package)
          '())))
    (home-page "https://sing-box.sagernet.org/")
    (synopsis "Universal proxy platform")
    (description
     "@command{sing-box} is a customizable and univsersal proxy platform that
can be used to create network proxy servers, clients and transparent proxies.")
    (license license:gpl3+)))

(define-public socks2http
  (package
    (name "socks2http")
    (version "0.0.0-20160712034938-bafa2cde8eb4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zenhack/socks2http")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c388rir9d0cy5vxqxj7m72nra0w5cya4mmgqdcvqmnk2vawdyb9"))))
    (build-system go-build-system)
    (arguments
     (list #:install-source? #f
           #:import-path "github.com/zenhack/socks2http"))
    (inputs (list go-golang-org-x-net))
    (home-page "https://github.com/zenhack/socks2http")
    (synopsis "SOCKS5 to HTTP proxy")
    (description
     "This package provides a simple tool to plumb HTTP proxy requests through
a SOCKS5 proxy.")
    (license license:expat)
    (properties
     '((disable-updater? . #t)))))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.94.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n5k8adplcffjwlpiy35nrklc0sf9ms18zlilcis5g1s3kdh4klf"))
              (patches
               (rosenthal-patches
                "tailscale-set-guix-system-PATH-for-SSH.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "tool")
                  (substitute* "net/tstun/tun_linux.go"
                    (("/sbin/(modprobe)" _ cmd) cmd))))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? (not (%current-target-system)) ;TODO: Run test suite.
      #:go go-1.25
      #:install-source? #f
      #:import-path "."
      #:build-flags
      #~(list "-tags" "ts_include_cli"
              (string-append
               "-ldflags="
               " -X tailscale.com/version.longStamp="
               #$(package-version this-package)
               " -X tailscale.com/version.shortStamp="
               #$(package-version this-package)))
      #:modules
      '((ice-9 match)
        ((guix build gnu-build-system) #:prefix gnu:)
        (guix build go-build-system)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda args
              (unsetenv "GO111MODULE")
              (apply (assoc-ref gnu:%standard-phases 'unpack) args)
              (copy-recursively
               #+(this-package-native-input "vendored-go-dependencies")
               "vendor")))
          (replace 'install-license-files
            (assoc-ref gnu:%standard-phases 'install-license-files))
          (replace 'build
            (lambda* (#:key build-flags parallel-build? #:allow-other-keys)
              (let* ((njobs (if parallel-build? (parallel-job-count) 1)))
                (setenv "GOMAXPROCS" (number->string njobs))
                (for-each
                 (lambda (pkg)
                   (apply invoke "go" "build" "-ldflags=-s -w" "-trimpath"
                          "-o" (string-append #$output "/bin/" pkg)
                          `(,@build-flags
                            ,(string-append "tailscale.com/cmd/" pkg))))
                 '("derper"
                   "derpprobe"
                   "tailscaled"
                   "tsidp")))))
          (add-after 'install 'install-extras
            (lambda _
              (symlink (in-vicinity #$output "bin/tailscaled")
                       (in-vicinity #$output "bin/tailscale"))
              (let ((tailscale
                     (or (which "tailscale")
                         (in-vicinity #$output "bin/tailscale"))))
                (map
                 (match-lambda
                   ((shell . path)
                    (let ((file (in-vicinity #$output path)))
                      (mkdir-p (dirname file))
                      (with-output-to-file file
                        (lambda ()
                          (invoke tailscale "completion" shell))))))
                 '(("bash" . "etc/bash_completion.d/tailscale")
                   ("fish" . "share/fish/vendor_completions.d/tailscale.fish")
                   ("zsh"  . "share/zsh/site-functions/_tailscale"))))))
          (add-after 'install 'wrap-binaries
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (in-vicinity #$output "bin/tailscaled")
                `("PATH" ":" prefix
                  ,(map (lambda (cmd)
                          (dirname (search-input-file inputs cmd)))
                        '("bin/find"
                          "bin/getent"
                          "bin/modprobe"
                          "sbin/ip"
                          "sbin/iptables"
                          "sbin/resolvconf"
                          "sbin/sysctl"))))))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (for-each
                 (lambda (cmd)
                   (invoke (string-append #$output "/bin/" cmd) "--help"))
                 '("derper"
                   "derpprobe"
                   "tailscaled"
                   "tsidp"))))))))
    (native-inputs
     (append
      (list (origin
              (method (go-mod-vendor #:go go-1.25))
              (uri (package-source this-package))
              (file-name "vendored-go-dependencies")
              (sha256
               (base32
                "04zzwizns5a5wvwpa1a82jg0l641fnk3schffrpqi3qi84x17qsr"))))
      (if (%current-target-system)
          (list this-package)
          '())))
    (inputs
     (list findutils glibc iproute iptables-nft kmod openresolv procps))
    (home-page "https://tailscale.com/")
    (synopsis "Mesh VPN service utilizing the WireGuard protocol and 2FA")
    (description
     "Tailscale is a mesh VPN service that simplifies the process of securely
connecting devices and services across various networks.  It allows you to
create a private network with minimal configuration and aims to remove the
complexity of building a trusted and secure network.")
    (license license:bsd-3)))
