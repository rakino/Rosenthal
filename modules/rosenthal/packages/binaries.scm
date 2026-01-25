;;; SPDX-FileCopyrightText: 2023-2024 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages binaries)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu build icecat-extension)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
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
    (version "2025.10.186.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkg.cloudflareclient.com"
                                  "/pool/bookworm/main/c/cloudflare-warp/"
                                  "cloudflare-warp_" version "_amd64.deb"))
              (sha256
               (base32
                "1gdfgrf5h0y37dsxzk7v9ndggavif5mmh1rp3p6sc5c4qb9y00y4"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("bin" "bin" #:include ("warp-cli" "warp-svc")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-deb
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "ar" "-x" source)
                   (invoke "tar" "-xf" "data.tar.gz")))
               (add-after 'install 'patch-elf
                 (lambda _
                   (let ((ld.so (string-append #$(this-package-input "glibc")
                                               #$(glibc-dynamic-linker)))
                         (rpath (string-join
                                 (list
                                  (string-append
                                   (ungexp
                                    (this-package-input "gcc") "lib") "/lib")
                                  (string-append
                                   #$(this-package-input "dbus") "/lib")
                                  (string-append
                                   #$(this-package-input "glibc") "/lib")
                                  (string-append
                                   #$(this-package-input "nspr") "/lib")
                                  (string-append
                                   #$(this-package-input "nss") "/lib/nss"))
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
                                (string-append #$output "/bin")))))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list patchelf-0.16))
    (inputs (list dbus `(,gcc "lib") glibc nspr nss))
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

(define-public conduit-bin
  (package
    (name "conduit-bin")
    (version "0.10.9-0.29aca17")
    (source (origin
              (method url-fetch)
              (uri "https://gitlab.com/famedly/conduit/-/jobs/11609473010/artifacts/raw/x86_64-unknown-linux-musl")
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0dqgcp5x6zvvqwiqb9vfw06dkb4ra3z8szn7cplna552h9nxfwh0"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'((#$(format #f "~a-~a"
                          (package-name this-package)
                          (package-version this-package))
                "bin/conduit"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'fix-permission
                 (lambda _
                   (chmod (string-append #$output "/bin/conduit") #o555))))))
    (supported-systems '("x86_64-linux"))
    (synopsis "Matrix homeserver")
    (description
     "Conduit aims to be an efficient Matrix homeserver that's easy to set up
and just works.  You can install it on a mini-computer like the Raspberry Pi to
host Matrix for your family, friends or company.")
    (home-page "https://conduit.rs/")
    (license license:asl2.0)
    (properties '((disable-updater? . #t)))))

(define-public tuwunel-bin
  (package
    (name "tuwunel-bin")
    (version "1.4.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/matrix-construct/tuwunel"
                    "/releases/download/v" version "/v" version
                    "-release-all-x86_64-v1-linux-gnu-tuwunel.zst"))
              (file-name (string-append name "-" version ".zst"))
              (sha256
               (base32
                "078d978xy84fw42y7wkl97gmfqyfn2j3vhh8k7ywkgh9lhi30mbn"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'((#$(format #f "~a-~a"
                          (package-name this-package)
                          (package-version this-package))
                "bin/tuwunel"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'fix-permission
                 (lambda _
                   (chmod (string-append #$output "/bin/tuwunel") #o555))))))
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
   (version "1.23.6")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/gotson/komga/releases/download/" version
                   "/komga-" version ".jar"))
             (sha256
              (base32
               "01mkhdxd2gh75g46381j5c4frys1bq5rywmb700ma5bdy7vd8k9i"))))
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
    (version "0.59.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/navidrome/navidrome/releases/download/v"
                    version "/navidrome_" version "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "1fn8hj6hm72nsyw69l5asqd1hm13z564ikk9v7nzg417553asx0h"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("navidrome" "bin/"))))
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
    (version "2.17.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muety/wakapi/releases/download/"
                    version "/wakapi_linux_amd64.zip"))
              (sha256
               (base32
                "0gb75wlip98187wc933p1cabsghh9w6lrh87nk3f2nhy86i2b5m7"))))
    (build-system copy-build-system)
    (arguments (list #:install-plan #~'(("wakapi" "bin/wakapi"))))
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
    (version "1.124.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/wakatime/wakatime-cli"
                                  "/releases/download/v" version
                                  "/wakatime-cli-linux-amd64.zip"))
              (sha256
               (base32
                "16glv2x0n118dczgjfaz02dwq70ngcq4wqc3pswdjzkch79z109m"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("wakatime-cli-linux-amd64" "bin/wakatime-cli"))))
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
    (version "12.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.grafana.com/grafana/release/"
                                  version "/grafana_" version "_" "16903967602"
                                  "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "056jj4ww1l36y77v9qmqhgsg7lsr328bhp7y48c6l125cal1snl2"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("bin" "bin")
               ("conf" "share/grafana/")
               ("public" "share/grafana/"))))
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
    (version "3.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/prometheus/prometheus"
                                  "/releases/download/v" version
                                  "/prometheus-" version ".linux-amd64.tar.gz"))
              (sha256
               (base32
                "116569xwfma38j7wbdz7r3hlnk9iz9ycg4yymjadkjxassfrk9l6"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("prometheus" "bin/")
               ("promtool" "bin/")
               ("prometheus.yml" "etc/"))))
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
    (version "2.17.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/mimir/releases/download/mimir-"
                    version "/mimir-linux-amd64"))
              (sha256
               (base32
                "1vnrpzwyjz7plzdiih65853ndvg64a9n1x1i7jqr085byhpayp82"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda* (#:key source #:allow-other-keys)
                   (let ((name "mimir")
                         (dest (in-vicinity #$output "bin")))
                     (mkdir-p dest)
                     (with-directory-excursion dest
                       (copy-file source name)
                       (chmod name #o555))))))))
    (synopsis "Scalable long-term storage for Prometheus")
    (description
     "Grafana Mimir provides horizontally scalable, highly available,
multi-tenant, long-term storage for Prometheus.")
    (home-page "https://grafana.com/oss/mimir/")
    (license license:agpl3)
    (supported-systems '("x86_64-linux"))
    (properties '((upstream-name . "mimir")))))

(define-public loki-bin
  (package
    (name "loki-bin")
    (version "3.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/loki/releases/download/v"
                    version "/loki-linux-amd64.zip"))
              (sha256
               (base32
                "1z1z60ki4zavijw0idpard0xx38q8140wv2hykxb3rikb05z0frk"))))
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

(define-public alloy-bin
  (package
    (name "alloy-bin")
    (version "1.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/alloy/releases/download/v"
                    version "/alloy-linux-amd64.zip"))
              (sha256
               (base32
                "03hwmnkx2awxlfw3ixplfnwzx7n1x624n1yw6cgky4hhjz13d3i8"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("alloy-linux-amd64" "bin/alloy"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'patch-elf
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((name "alloy")
                         (dest (in-vicinity #$output "bin"))
                         (ld.so (search-input-file inputs #$(glibc-dynamic-linker))))
                     (with-directory-excursion dest
                       (invoke "patchelf" "--set-interpreter" ld.so name))))))))
    (native-inputs (list patchelf unzip))
    (synopsis
     "OpenTelemetry Collector distribution with programmable pipelines")
    (description
     "Grafana Alloy is an open source OpenTelemetry Collector distribution with
built-in Prometheus pipelines and support for metrics, logs, traces, and
profiles.")
    (home-page "https://grafana.com/oss/alloy-opentelemetry-collector/")
    (license license:agpl3)
    (supported-systems '("x86_64-linux"))
    (properties '((upstream-name . "alloy")))))

(define-public alloy-bin-aarch64-linux
  (package
    (inherit alloy-bin)
    (name "alloy-bin-aarch64-linux")
    (version "1.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/grafana/alloy/releases/download/v"
                    version "/alloy-linux-arm64.zip"))
              (sha256
               (base32
                "1gnfdhs8rxyn18swy1kv1f2lbsj6abjlhrgaibsj2a87swgcyvjg"))))
    (arguments
     (substitute-keyword-arguments (package-arguments alloy-bin)
       ((#:install-plan _ ''())
        #~'(("alloy-linux-arm64" "bin/alloy")))))
    (supported-systems '("aarch64-linux"))))
