;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages browser-extensions)
  ;; Guile builtins
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (gnu build icecat-extension)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system copy)
  ;; Guix packages
  #:use-module (gnu packages compression))

(define bitwarden
  (package
    (name "bitwarden")
    (version "2026.2.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/bitwarden/clients"
                                  "/releases/download/browser-v" version
                                  "/dist-firefox-" version ".zip"))
              (sha256
               (base32
                "1l2mabgbxf2jdsgd56fbalzszjkxp03cxba1r15vgvdlgmkif5cl"))))
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
       (disable-updater? . #t)))))

(define-public bitwarden/icecat
  (let ((base (make-icecat-extension bitwarden)))
    (package
      (inherit base)
      (properties
       `(,@(alist-delete 'hidden? (package-properties base))
         (disable-updater? . #t))))))

(define-public miniflux-injector
  (package
    (name "miniflux-injector")
    (version "2.3.3")
    (properties
     '((addon-id . "{528ec801-2e29-4cb9-ae71-5a90503138d1}")
       (hidden? . #t)
       (disable-updater? . #t)))
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
         (disable-updater? . #t))))))

(define ohmyech
  (package
    (name "ohmyech")
    (version "1.2")
    (properties
     '((addon-id . "{46b8ab0b-8adf-4e43-ad67-acef5a8d45c9}")
       (hidden? . #t)
       (disable-updater? . #t)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/27justin/ohmyech")
                    (commit "aa0cf4cc5c4bcb6ce508d8a4e7ee4c95c2e63cbe")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cqgfv2q7shisi9ijac5s85xy9q2rz9f9hxwq3q41vfgrcvd8zng"))
              (modules '((guix build utils)))
              (snippet
               #~(substitute* "manifest.json"
                   (("\"version\".*" line)
                    (string-append line "\
  \"browser_specific_settings\": {
    \"gecko\": {
      \"id\": \"" #$(assq-ref properties 'addon-id) "\"
    }
  },
"))))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." #$(assq-ref (package-properties this-package) 'addon-id)))))
    (home-page "https://github.com/27justin/ohmyech")
    (synopsis "Visual indicator for Encrpted Client Hello")
    (description
     "OhMyECH is a browser extension for indicating the use of @acronym{ECH,
Encrypted Client Hello} on web pages.  When enabled, it adds an icon to the
browser address bar, providing users with a visual cue about whether the
current page employs @acronym{ECH}, which helps protect sensitive information
during the @acronym{TLS, Transport Layer Security} handshake process.")
    (license license:expat)))

(define-public ohmyech-icecat
  (let ((base (make-icecat-extension ohmyech)))
    (package
      (inherit base)
      (properties
       `(,@(alist-delete 'hidden? (package-properties base))
         (disable-updater? . #t))))))
