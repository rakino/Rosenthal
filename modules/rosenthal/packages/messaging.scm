;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages messaging)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system pyproject)
  #:use-module (rosenthal build-system go-vendored)
  ;; Guix packages
  #:use-module (gnu packages bash)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages video)
  #:use-module (rosenthal packages animation)
  #:use-module (rosenthal packages python-xyz))

(define-public heisenbridge
  (package
    (name "heisenbridge")
    (version "1.15.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hifi/heisenbridge")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1viv18bkj0lccr00218xp818i923h1ax7c50kl0yn834axdz6y4l"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;TODO
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-aiohttp
           python-irc
           python-mautrix
           python-ruamel.yaml
           python-socks))
    (synopsis "Bouncer-style Matrix IRC bridge")
    (description
     "Heisenbridge brings IRC to Matrix by creating an environment where every
user connects to each network individually like they would with a traditional
IRC bouncer.  Simplicity is achieved by exposing IRC in the most straightforward
way as possible where it makes sense so it feels familiar for long time IRC
users.")
    (home-page "https://github.com/hifi/heisenbridge")
    (license license:expat)))

(define-public mautrix-telegram
  (package
    (name "mautrix-telegram")
    (version "0.2606.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mautrix/telegram")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08agcpd5na1l7j9ww03z9axwcayfidz4ac93zn9x0lm4c2s2maml"))))
    (build-system go-vendored-build-system)
    (arguments
     (list #:go go-1.26
           #:vendor-hash
           (base32 "0jmmn2bh8spjcc39q9l7z220vlbdk2plhy8jlqmk6rsi1qkdsl7r")
           #:install-source? #f
           #:import-path "./cmd/mautrix-telegram"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-binary
                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-program (in-vicinity #$output "bin/mautrix-telegram")
                     `("PATH" ":" prefix
                       ,(map (lambda (cmd)
                               (dirname (search-input-file inputs cmd)))
                             '("bin/ffmpeg"
                               "bin/lottieconverter")))))))))
    (inputs (list bash-minimal ffmpeg lottie-converter olm))
    (synopsis "Matrix-Telegram hybrid puppeting/relaybot bridge")
    (description
     "This package provides a Matrix-Telegram hybrid puppeting/relaybot
bridge.")
    (home-page "https://github.com/mautrix/telegram")
    (license license:agpl3+)))
