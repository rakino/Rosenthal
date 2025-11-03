;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages messaging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages video)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal packages animation)
  #:use-module (rosenthal packages python-xyz))

(define-public mautrix-telegram
  (package
    (name "mautrix-telegram")
    (version "0.15.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mautrix/telegram")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ivhp827ypngq3p8m5bmiv1nk2x2c4y0z1c37rczjmq941dnlw63"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f                  ;No test suite.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-extras
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((bindir (in-vicinity #$output "bin"))
                          (etcdir (in-vicinity #$output "etc/mautrix-telegram"))
                          (bin (in-vicinity bindir "mautrix-telegram"))
                          (cfg (in-vicinity #$output "example-config.yaml")))
                     (for-each mkdir-p (list bindir etcdir))
                     (install-file cfg etcdir)
                     (delete-file cfg)
                     (call-with-output-file bin
                       (lambda (port)
                         (format port "~
#!/bin/sh
PATH=~a:~a:$PATH
~a -m mautrix_telegram \"$@\"~%"
                                 (dirname (dirname (search-input-file inputs "bin/ffmpeg")))
                                 (dirname (dirname (search-input-file inputs "bin/lottieconverter")))
                                 (search-input-file inputs "bin/python3"))))
                     (chmod bin #o555)))))))

    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-asyncpg
           python-commonmark
           python-aiosqlite
           python-magic
           python-mako
           python-mautrix
           python-ruamel.yaml
           python-tulir-telethon))
    (inputs
     (list ffmpeg
           lottie-converter
           python
           python-aiodns
           python-aiosqlite
           python-brotli
           python-olm
           python-phonenumbers
           python-pillow
           python-prometheus-client
           python-pycryptodome
           python-qrcode
           python-socks
           python-unpaddedbase64))
    (synopsis "Matrix-Telegram hybrid puppeting/relaybot bridge")
    (description
     "This package provides a Matrix-Telegram hybrid puppeting/relaybot
bridge.")
    (home-page "https://github.com/mautrix/telegram")
    (license license:agpl3+)))
