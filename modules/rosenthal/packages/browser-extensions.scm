;; SPDX-FileCopyrightText: 2023-2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages browser-extensions)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu build icecat-extension))

(define-public ohmyech
  (package
    (name "ohmyech")
    (version "1.1")
    (properties
     '((addon-id . "{46b8ab0b-8adf-4e43-ad67-acef5a8d45c9}")
       (hidden? . #t)
       (rosenthal-update? . #f)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/27justin/ohmyech")
                    (commit "ec7935d500a9d354776586e25261cef3595c40c7")))
              (sha256
               (base32
                "1mrbm8c8z9zpfrs0qk5qj64f35p7c5lrxfn3nhda0ar0b2rv6593"))
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
         (rosenthal-update? . #f))))))
