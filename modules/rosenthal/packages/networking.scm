;; SPDX-FileCopyrightText: 2022-2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages networking)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (rosenthal utils download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages linux))

(define-public cloudflared
  (package
    (name "cloudflared")
    (version "2025.4.2")
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
                "0mblq1zvl7722k3515yc99ym18li39anlcmj8s10m7kkp9yfc596"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.23
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
    (license license:expat)))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.80.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07s8kwksvd0f9r65zkrhp3sn4jrv0c8g5w0wbiv9qq950l8gdv2h"))
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
      #:go go-1.23
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
              (method (go-mod-vendor #:go go-1.23))
              (uri (package-source this-package))
              (file-name "vendored-go-dependencies")
              (sha256
               (base32
                "1lp5xqb9nmz1dqmmvdnnl0qla7zw6v25jbyf6shrl65rh270wmgk"))))
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
