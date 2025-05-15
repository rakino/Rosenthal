;; SPDX-FileCopyrightText: 2023-2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages binaries)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix download)
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
  #:use-module (rosenthal packages networking)
  #:use-module (rosenthal packages rust-apps)
  #:use-module (rosenthal packages web))

(define license
  (@@ (guix licenses) license))

(define-public atuin-bin
  (deprecated-package "atuin-bin" atuin))

(define-public bitwarden
  (package
    (name "bitwarden")
    (version "2025.3.1")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/bitwarden/clients"
                                  "/releases/download/browser-v" version
                                  "/dist-firefox-" version ".zip"))
              (sha256
               (base32
                "03s8z32rc4mwzi61xpzn4f9z6kxwdnshqy61h2kr3cvq9974li3s"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." #$(assq-ref (package-properties this-package) 'addon-id)))))
    (home-page "https://bitwarden.com/")
    (synopsis "Bitwarden client browser extension")
    (description
     "This package provides browser extension for Bitwarden client.")
    (license license:gpl3)
    (properties
     '((addon-id . "{446900e4-71c2-419f-a6a7-df9c091e268b}")
       (hidden? . #t)
       (rosenthal-update? . #f)))))

(define-public bitwarden/icecat
  (let ((base (make-icecat-extension bitwarden)))
    (package
      (inherit base)
      (properties
       `(,@(alist-delete 'hidden? (package-properties base))
         (rosenthal-update? . #f))))))

(define-public hugo-bin
  (deprecated-package "hugo-bin" hugo))

(define-public mihomo-bin
  (deprecated-package "mihomo-bin" mihomo))

(define-public clash-meta-bin
  (deprecated-package "clash-meta-bin" mihomo-bin))

(define-public cloudflare-warp-bin
  (package
    (name "cloudflare-warp-bin")
    (version "2025.4.929.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkg.cloudflareclient.com"
                                  "/pool/bookworm/main/c/cloudflare-warp/"
                                  "cloudflare-warp_" version "_amd64.deb"))
              (sha256
               (base32
                "1ygqr6l7lsbkq1f8qwgypv2flyjd430sjkrwsw8rhd2lgnvwisff"))))
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

(define-public komga-bin
  (package
   (name "komga-bin")
   (version "1.21.2")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/gotson/komga/releases/download/" version
                   "/komga-" version ".jar"))
             (sha256
              (base32
               "0ya31gjikr71m8mc2pagm1pgc951q6q6yi7mvpa47qskspbkk30m"))))
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
   (license license:expat)
   (properties '((upstream-name . "komga")))))

(define-public miniflux-injector
  (package
    (name "miniflux-injector")
    (version "2.3.3")
    (properties
     '((addon-id . "{528ec801-2e29-4cb9-ae71-5a90503138d1}")
       (hidden? . #t)
       (rosenthal-update? . #f)))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/Sevichecc/miniflux-injector/releases/download"
             "/v" version "/miniflux_injector-" version ".zip"))
       (sha256
        (base32
         "199z441ak6dwy7skgbwc9aa4gfd2r4i22hxfm27s5k3rv7barbvs"))
       (modules '((guix build utils)))
       (snippet
        #~(substitute* "manifest.json"
            (("homepage_url.*" line)
             (string-append line "\
  \"browser_specific_settings\": {
    \"gecko\": {
      \"id\": \"" #$(assq-ref properties 'addon-id) "\"
    }
  },
"))))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." #$(assq-ref (package-properties this-package) 'addon-id)))))
    (home-page "https://github.com/Sevichecc/miniflux-injector")
    (synopsis "Injects Miniflux search results into search page")
    (description
     "This package provides a browser extension to inject Miniflux search
results into search page.  Search terms are sent to your Miniflux instance and
results are added in a sidebar next to search engine results.")
    (license license:expat)))

(define-public miniflux-injector/icecat
  (let ((base (make-icecat-extension miniflux-injector)))
    (package
      (inherit base)
      (properties
       `(,@(alist-delete 'hidden? (package-properties base))
         (rosenthal-update? . #f))))))

(define-public navidrome-bin
  (package
    (name "navidrome-bin")
    (version "0.55.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/navidrome/navidrome/releases/download/v"
                    version "/navidrome_" version "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "0h3984p10am39y619ibrvk1g96ra52kig929n792399q2jw1lrlp"))))
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

(define-public sing-box-bin
  (deprecated-package "sing-box-bin" sing-box))

(define-public tailscale-bin
  (deprecated-package "tailscale-bin" tailscale))

(define-public wakapi-bin
  (package
    (name "wakapi-bin")
    (version "2.13.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muety/wakapi/releases/download/"
                    version "/wakapi_linux_amd64.zip"))
              (sha256
               (base32
                "07wylvgi8yqcmywpvgbsqyhza86nmg8dfx1apmaynlw80y0nzial"))))
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
    (version "1.115.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/wakatime/wakatime-cli"
                                  "/releases/download/v" version
                                  "/wakatime-cli-linux-amd64.zip"))
              (sha256
               (base32
                "012s2r55iwd8kglh26sbscaps0iccdhk0ccaqf55kcfsbi35qc91"))))
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
