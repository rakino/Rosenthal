;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2024 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages binaries)
  ;; Guile builtins
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (gnu build icecat-extension)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (rosenthal utils packages)
  ;; Guix origin methods
  #:use-module (guix download)
  ;; Guix build systems
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  ;; Guix packages
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages java)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages sync)
  #:use-module (rosenthal packages networking)
  #:use-module (rosenthal packages rust-apps)
  #:use-module (rosenthal packages web))

(define license
  (@@ (guix licenses) license))

(define-public cloudflare-warp-bin
  (package
    (name "cloudflare-warp-bin")
    (version "2026.6.880.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkg.cloudflareclient.com"
                                  "/pool/bookworm/main/c/cloudflare-warp/"
                                  "cloudflare-warp_" version "_amd64.deb"))
              (sha256
               (base32
                "1wf2p09c1ak4mnnpl5pgnwlfy1x4nmhvsfnapyj1gdys93w4v7iy"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? (not (%current-target-system))
      #:imported-modules
      (append %default-gnu-imported-modules
              %copy-build-system-modules)
      #:modules
      '((guix build utils)
        (guix build gnu-build-system)
        ((guix build copy-build-system) #:prefix copy:))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (add-after 'unpack 'unpack-deb
            (lambda* (#:key source #:allow-other-keys)
              (invoke "ar" "-x" source)
              (invoke "tar" "-xf" "data.tar.gz")))
          (replace 'install
            (lambda args
              (apply (assoc-ref copy:%standard-phases 'install)
                     #:install-plan
                     '(("bin" "bin" #:include ("warp-cli" "warp-svc")))
                     args)))
          (add-after 'install 'patch-elf
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((ld.so (search-input-file inputs #$(glibc-dynamic-linker)))
                     (rpath
                      (string-join
                       (cons* (dirname ld.so)
                              (dirname
                               (search-input-file inputs "lib/nss/libnss3.so"))
                              (map (lambda (lib)
                                     (dirname (search-input-file
                                               inputs (in-vicinity "lib" lib))))
                                   '("libdbus-1.so"
                                     "libgcc_s.so"
                                     "libnspr4.so"
                                     "libtss2-esys.so")))
                       ":")))
                (define (patch-elf file)
                  (format #t "Patching ~a ..." file)
                  (unless (string-contains file ".so")
                    (invoke "patchelf" "--set-interpreter" ld.so file))
                  (invoke "patchelf" "--set-rpath" rpath file)
                  (display " done\n"))
                (for-each (lambda (file)
                            (patch-elf file))
                          (find-files
                           (string-append #$output "/bin"))))))
          (add-after 'patch-elf 'check
            (lambda* (#:key tests? outputs #:allow-other-keys)
              (let ((cmd (search-input-file outputs "bin/warp-cli")))
                (when tests?
                  (invoke cmd "--help")
                  (invoke cmd "--version"))))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list patchelf-0.16))
    (inputs (list dbus `(,gcc "lib") glibc nspr nss tpm2-tss))
    (home-page "https://1.1.1.1/")
    (synopsis "Cloudflare WARP client")
    (description
     "The Cloudflare WARP client allows individuals to have a faster, more
secure, and more private experience online.  The WARP client sits between your
device and the Internet, and has several connection modes to better suit
different needs.")
    (license
     (license "Nonfree"
              "https://www.cloudflare.com/application/terms/"
              "This is a nonfree license.  Check the URI for details."))
    (properties
     '((upstream-name . "cloudflare-warp")
       (release-monitoring-url
        . "https://pkg.cloudflareclient.com/dists/bookworm/main/binary-amd64/Packages")))))

(define-public tuwunel-bin
  (package
    (name "tuwunel-bin")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/matrix-construct/tuwunel"
                    "/releases/download/v" version "/v" version
                    "-release-all-x86_64-v1-linux-gnu-tuwunel.zst"))
              (file-name (string-append name "-" version ".zst"))
              (sha256
               (base32
                "0v6dlarsdr8rnj3gr7m3la0hv9xv18cyc4jx46qy0lljiwjrcdw2"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:imported-modules
           (append %default-gnu-imported-modules
                   %copy-build-system-modules)
           #:modules
           '((guix build utils)
             (guix build gnu-build-system)
             ((guix build copy-build-system) #:prefix copy:))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (delete 'check)
               (replace 'install
                 (lambda args
                   (apply (assoc-ref copy:%standard-phases 'install)
                          #:install-plan
                          '((#$(format #f "~a-~a"
                                       (package-name this-package)
                                       (package-version this-package))
                             "bin/tuwunel"))
                          args)))
               (add-after 'install 'fix-permission
                 (lambda _
                   (chmod (string-append #$output "/bin/tuwunel") #o555)))
               (add-after 'fix-permission 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((tuwunel (search-input-file outputs "bin/tuwunel")))
                     (when tests?
                       (invoke tuwunel "--help")
                       (invoke tuwunel "--version"))))))))
    (supported-systems '("x86_64-linux"))
    (synopsis "Matrix homeserver")
    (description
     "Tuwunel is a featureful Matrix homeserver you can use instead of Synapse
with your favorite client, bridge or bot.  It is written entirely in Rust to be
a scalable, lightweight, low-cost, community-driven alternative covering all but
the most niche uses.")
    (home-page "https://matrix-construct.github.io/tuwunel/")
    (license license:asl2.0)
    (properties '((upstream-name . "tuwunel")
                  (disable-updater? . #t)))))

(define-public komga-bin
  (package
   (name "komga-bin")
   (version "1.26.3")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/gotson/komga/releases/download/" version
                   "/komga-" version ".jar"))
             (sha256
              (base32
               "09vrygb58zpngq9bp4zl3ip8h5am39nlni5igj6hpirw8p0v2g0n"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan
          #~'((#$(string-append "komga-" (package-version this-package) ".jar")
               "lib/komga/komga.jar"))
          #:phases
          #~(modify-phases %standard-phases
              (replace 'install
                (lambda* (#:key inputs source #:allow-other-keys)
                  (let* ((lib (in-vicinity #$output "lib/komga"))
                         (bin (in-vicinity #$output "bin"))
                         (jar (in-vicinity lib "komga.jar"))
                         (exe "komga"))
                    (mkdir-p lib)
                    (copy-file source jar)
                    (call-with-output-file exe
                      (lambda (port)
                        (format port "~
#!~a
export LC_ALL=C.UTF-8
exec ~a -jar ~a $@~%"
                                (search-input-file inputs "bin/bash")
                                (search-input-file inputs "bin/java")
                                jar)))
                    (chmod exe #o555)
                    (install-file exe bin)))))))
   (inputs (list bash-minimal openjdk))
   (home-page "https://komga.org/")
   (synopsis "Media server for comics/mangas/BDs/magazines/eBooks")
   (description
    "Komga is a media server for your comics, mangas, BDs, magazines and
eBooks.")
   (supported-systems '("x86_64-linux"))
   (license license:expat)
   (properties '((upstream-name . "komga")))))

(define-public navidrome-bin
  (package
    (name "navidrome-bin")
    (version "0.63.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/navidrome/navidrome/releases/download/v"
                    version "/navidrome_" version "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "1w6lvqyhlgf4xi1jybpgnn5y6ga2cs39i5vv734sj4fcwmpnsk12"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:imported-modules
           (append %default-gnu-imported-modules
                   %copy-build-system-modules)
           #:modules
           '((guix build utils)
             (guix build gnu-build-system)
             ((guix build copy-build-system) #:prefix copy:))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (delete 'check)
               (replace 'install
                 (lambda args
                   (apply (assoc-ref copy:%standard-phases 'install)
                          #:install-plan '(("navidrome" "bin/"))
                          args)))
               (add-after 'install 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((cmd (search-input-file outputs "bin/navidrome")))
                     (when tests?
                       (invoke cmd "--help")
                       (invoke cmd "--version"))))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.navidrome.org/")
    (synopsis "Web-based music collection server and streamer")
    (description
     "Navidrome is a self-hosted music server that allows users to stream and
manage their music collections.  It provides a web interface and is compatible
with the Subsonic API.")
    (license license:expat)
    (properties '((upstream-name . "navidrome")))))

(define-deprecated-package rclone-bin rclone)

(define-public shadow-tls-bin
  (package
    (name "shadow-tls-bin")
    (version "0.2.25")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ihciah/shadow-tls/releases/download/v"
                    version "/shadow-tls-x86_64-unknown-linux-musl"))
              (sha256
               (base32
                "0chmqzfmyw5w8ybshkwigc3r25svq7fyw371d0dj2ibzsprgawx1"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("shadow-tls-x86_64-unknown-linux-musl" "bin/shadow-tls"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'fix-permission
                 (lambda _
                   (chmod (string-append #$output "/bin/shadow-tls") #o555))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.ihcblog.com/a-better-tls-obfs-proxy/")
    (synopsis "Proxy to expose real tls handshake to the firewall")
    (description
     "Shadow TLS is a proxy to expose real tls handshake to the @acronym{MITM,
monster-in-the-middle}.")
    (license license:expat)
    (properties '((upstream-name . "shadow-tls")))))

(define-public wakapi-bin
  (package
    (name "wakapi-bin")
    (version "2.17.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muety/wakapi/releases/download/"
                    version "/wakapi_linux_amd64.zip"))
              (sha256
               (base32
                "09cm35kg5zpw6b882ws1j6nrvb3pdg23ndklr60li7nzsddh6ggf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:imported-modules
           (append %default-gnu-imported-modules
                   %copy-build-system-modules)
           #:modules
           '((guix build utils)
             (guix build gnu-build-system)
             ((guix build copy-build-system) #:prefix copy:))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (delete 'check)
               (replace 'install
                 (lambda args
                   (apply (assoc-ref copy:%standard-phases 'install)
                          #:install-plan '(("wakapi" "bin/wakapi"))
                          args)))
               (add-after 'install 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((cmd (search-input-file outputs "bin/wakapi")))
                     (when tests?
                       (invoke cmd "--help")
                       (invoke cmd "--version"))))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list unzip))
    (home-page "https://wakapi.dev/")
    (synopsis "WakaTime-compatible backend")
    (description
     "This package provides @code{wakapi}, a WakaTime-compatible backend for
coding statistics.")
    (license license:expat)
    (properties '((upstream-name . "wakapi")))))

(define-public wakatime-cli-bin
  (package
    (name "wakatime-cli-bin")
    (version "2.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/wakatime/wakatime-cli"
                                  "/releases/download/v" version
                                  "/wakatime-cli-linux-amd64.zip"))
              (sha256
               (base32
                "19g1y19k76pmnzi7irsk1jb7dvrqjd5884434x1s441v4zz6bynz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:imported-modules
           (append %default-gnu-imported-modules
                   %copy-build-system-modules)
           #:modules
           '((guix build utils)
             (guix build gnu-build-system)
             ((guix build copy-build-system) #:prefix copy:))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (delete 'check)
               (replace 'install
                 (lambda args
                   (apply (assoc-ref copy:%standard-phases 'install)
                          #:install-plan
                          '(("wakatime-cli-linux-amd64" "bin/wakatime-cli"))
                          args)))
               (add-after 'install 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((cmd (search-input-file outputs "bin/wakatime-cli")))
                     (when tests?
                       (setenv "HOME" "/tmp")
                       (invoke cmd "--help")
                       (invoke cmd "--version"))))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list unzip))
    (home-page "https://wakatime.com/plugins")
    (synopsis "Command line interface to WakaTime")
    (description
     "This package provides @command{wakatime-cli}, the command line interface
to WakaTime, which is used by all WakaTime text editor plugins.")
    (license license:bsd-3)
    (properties '((upstream-name . "wakatime-cli")))))

(define-public grafana-bin
  (package
    (name "grafana-bin")
    (version "12.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.grafana.com/grafana/release/"
                                  version "/grafana_" version "_"
                                  "23531306697_linux_amd64.tar.gz"))
              (sha256
               (base32
                "07sr78wi5nqlsx1nfj87bzgq29hcflqm6r5f5xcn9gq3m00b4h7j"))))
    (build-system copy-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:install-plan
           #~'(("bin" "bin")
               ("conf" "share/grafana/")
               ("public" "share/grafana/"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((cmd (search-input-file outputs "bin/grafana")))
                     (when tests?
                       (invoke cmd "--help")
                       (invoke cmd "--version"))))))))
    (synopsis "Platform for monitoring and observability")
    (description
     "Grafana allows you to query, visualize, alert on and understand your
metrics no matter where they are stored.")
    (home-page "https://grafana.com/")
    (license license:agpl3)
    (supported-systems '("x86_64-linux"))
    (properties '((upstream-name . "grafana")
                  (disable-updater? . #t)))))

(define-public prometheus-bin
  (package
    (name "prometheus-bin")
    (version "3.13.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/prometheus/prometheus"
                                  "/releases/download/v" version
                                  "/prometheus-" version ".linux-amd64.tar.gz"))
              (sha256
               (base32
                "0vxy87m6knrpgwv1rgn1ix47zidbrb97gqv5hbm2bl0v2134v30f"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:imported-modules
           (append %default-gnu-imported-modules
                   %copy-build-system-modules)
           #:modules
           '((guix build utils)
             (guix build gnu-build-system)
             ((guix build copy-build-system) #:prefix copy:))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (delete 'check)
               (replace 'install
                 (lambda args
                   (apply (assoc-ref copy:%standard-phases 'install)
                          #:install-plan
                          '(("prometheus" "bin/")
                            ("promtool" "bin/")
                            ("prometheus.yml" "etc/"))
                          args)))
               (add-after 'install 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((cmd (search-input-file outputs "bin/prometheus")))
                     (when tests?
                       (invoke cmd "--help")
                       (invoke cmd "--version"))))))))
    (synopsis "Monitoring system and time series database")
    (description
     "Prometheus is a systems and service monitoring system.  It collects
metrics from configured targets at given intervals, evaluates rule expressions,
displays the results, and can trigger alerts when specified conditions are
observed.")
    (home-page "https://prometheus.io/")
    (license license:asl2.0)
    (supported-systems '("x86_64-linux"))
    (properties '((upstream-name . "prometheus")))))

(define-public mimir-bin
  (package
    (name "mimir-bin")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/mimir/releases/download/mimir-"
                    version "/mimir-linux-amd64"))
              (sha256
               (base32
                "09bhfbax659p1svzk8gq37vckqd62xsr219ab8f4r36c74ml3vz8"))))
    (build-system copy-build-system)
    (arguments
     (list #:tests? (not (%current-target-system))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda* (#:key source #:allow-other-keys)
                   (let ((name "mimir")
                         (dest (in-vicinity #$output "bin")))
                     (mkdir-p dest)
                     (with-directory-excursion dest
                       (copy-file source name)
                       (chmod name #o555)))))
               (add-after 'install 'check
                 (lambda* (#:key tests? outputs #:allow-other-keys)
                   (let ((cmd (search-input-file outputs "bin/mimir")))
                     (when tests?
                       (invoke cmd "-version"))))))))
    (synopsis "Scalable long-term storage for Prometheus")
    (description
     "Grafana Mimir provides horizontally scalable, highly available,
multi-tenant, long-term storage for Prometheus.")
    (home-page "https://grafana.com/oss/mimir/")
    (license license:agpl3)
    (supported-systems '("x86_64-linux"))
    (properties '((upstream-name . "mimir")
                  (disable-updater? . #t)))))

(define-public loki-bin
  (package
    (name "loki-bin")
    (version "3.7.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/loki/releases/download/v"
                    version "/loki-linux-amd64.zip"))
              (sha256
               (base32
                "1sj8a9nmva2lnvbca8x62wy2lwlhnahdr99rpw814n0nfm117lh9"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("loki-linux-amd64" "bin/loki"))))
    (native-inputs (list unzip))
    (synopsis "Log aggregation system")
    (description
     "Loki is a horizontally scalable, highly available, multi-tenant log
aggregation system inspired by Prometheus.  It is designed to be very cost
effective and easy to operate.  It does not index the contents of the logs, but
rather a set of labels for each log stream.")
    (home-page "https://grafana.com/oss/loki/")
    (license license:agpl3)
    (supported-systems '("x86_64-linux"))
    (properties '((upstream-name . "loki")))))


;;;
;;; alloy
;;;

(define-public %alloy-source-x86_64-linux
  (package
    (inherit %binary-source)
    (name "alloy")
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/alloy/releases/download/v"
                    version "/alloy-linux-amd64.zip"))
              (sha256
               (base32
                "0g28gg013a8i3d35aqfxhsp94hzp65dni6lg6si50flqqg5m7j7s"))))))

(define-public %alloy-source-aarch64-linux
  (package
    (inherit %binary-source)
    (name "alloy")
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/alloy/releases/download/v"
                    version "/alloy-linux-arm64.zip"))
              (sha256
               (base32
                "1mknhvxbppcg5x2lwk7832l6wlk2h7cm7y8ay34xc515wb7ld529"))))))

(define-public alloy-bin
  (binary-package
   `(("x86_64-linux"  . ,%alloy-source-x86_64-linux)
     ("aarch64-linux" . ,%alloy-source-aarch64-linux))
   (package
     (name "alloy-bin")
     (version "")
     (source #f)
     (build-system gnu-build-system)
     (arguments
      (list
       #:modules
       `((ice-9 match)
         ,@%default-gnu-modules)
       #:phases
       #~(modify-phases %standard-phases
           (delete 'bootstrap)
           (delete 'configure)
           (delete 'build)
           (delete 'check)
           (replace 'install
             (lambda _
               (let ((src #$(mapping-for-system
                             '(("x86_64-linux"  . "alloy-linux-amd64")
                               ("aarch64-linux" . "alloy-linux-arm64"))))
                     (dst (in-vicinity #$output "bin/alloy")))
                 (mkdir-p (dirname dst))
                 (copy-file src dst))))
           (add-after 'install 'patch-elf
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((ld.so (search-input-file inputs #$(glibc-dynamic-linker)))
                     (dest (in-vicinity #$output "bin"))
                     (name "alloy"))
                 (with-directory-excursion dest
                   (invoke "patchelf" "--set-interpreter" ld.so name)
                   (invoke "patchelf" "--set-rpath" (dirname ld.so) name)
                   (chmod name #o555)))))
           (add-after 'patch-elf 'install-extras
             (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
               (let ((alloy
                      (search-input-file
                       (if #$(%current-target-system)
                           native-inputs
                           outputs)
                       "bin/alloy")))
                 (for-each
                  (match-lambda
                    ((shell . file)
                     (mkdir-p (in-vicinity #$output (dirname file)))
                     (with-output-to-file (in-vicinity #$output file)
                       (lambda ()
                         (invoke alloy "completion" shell)))))
                  '(("bash" . "share/bash-completion/completions/alloy")
                    ("fish" . "share/fish/vendor_completions.d/alloy.fish")
                    ("zsh"  . "share/zsh/site-functions/_alloy")))))))))
     (native-inputs
      (append (if (%current-target-system)
                  (list this-package)
                  '())
              (list patchelf-0.16 unzip)))
     (supported-systems '("x86_64-linux" "aarch64-linux"))
     (home-page "https://grafana.com/oss/alloy-opentelemetry-collector/")
     (synopsis
      "OpenTelemetry Collector distribution with programmable pipelines")
     (description
      "Grafana Alloy is an open source OpenTelemetry Collector distribution with
built-in Prometheus pipelines and support for metrics, logs, traces, and
profiles.")
     (license license:agpl3))))

(define-deprecated-package alloy-bin-aarch64-linux alloy-bin)
