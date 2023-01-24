;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-company-dev
  (let ((revision "367")
        (commit "93b0284e5a6be8506d194aab00fb613be0f364fd"))
    (package
      (inherit emacs-company)
      (name "emacs-company-dev")
      (version (git-version "0.9.13" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/company-mode/company-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "002wp7q1fcjdavysbmil1d46v34rnkkgyhkm0wig8r1c9bk8qksp")))))))

(define-public emacs-doom-modeline-dev
  (let ((base emacs-doom-modeline)
        (revision "42")
        (commit "fc25051acca30237b0a8ebff50460f522904bf45"))
    (package
      (inherit base)
      (name "emacs-doom-modeline-dev")
      (version (git-version "3.3.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/seagle0128/doom-modeline")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pj4fkj2ksj9zcx391q2bnhzir72m9x153b75kmi3dg6nw8dpzrs"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (delete "emacs-all-the-icons" "emacs-dash"))))))

(define-public emacs-rime-dev
  (let ((revision "12")
        (commit "0a50c918d2de56aa401a68ba37394446c6fc9ed6"))
    (package
      (inherit emacs-rime)
      (name "emacs-rime-dev")
      (version (git-version "1.0.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/DogLooksGood/emacs-rime")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "15a15jz0rndkkayj384agfx5yd70nnvcr92la3ajh8a16fs8kk80")))))))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59552
(define-public emacs-wakatime-mode
  ;; No release since May 5, 2015.
  (let ((version "1.0.2") ;32a0154cd4bbd525d354997e6b12c6a9726d0b43, not tagged
        (revision "55")
        (commit "ef923829912c3854d230834f81083814b7c9d992"))
    (package
      (name "emacs-wakatime-mode")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wakatime/wakatime-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0m29817s2din4n42pflvzg2538i91s0n06y78pf8q94qf0mfspcl"))))
      (build-system emacs-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 ;; XXX: WakaTime hasn't packaged in Guix yet.
                 (delete 'patch-el-files))))
      (home-page "https://wakatime.com/emacs")
      (synopsis "Automatic time tracking extension for Emacs using WakaTime")
      (description
       "@code{wakatime-mode} is an Emacs plugin for automatic time tracking
and metrics generated from your programming activity.")
      (license license:gpl3+))))

(define-public emacs-org-rainbow-tags-dev
  (let ((revision "11")
        (commit "6001ec9345bea4e60b2178940ef197c055d5a5d8"))
    (package
      (inherit emacs-org-rainbow-tags)
      (name "emacs-org-rainbow-tags-dev")
      (version (git-version "0.1-pre" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/KaratasFurkan/org-rainbow-tags")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qlpszfvi1zngfi377pxkb9byhw0x8h4wsc70jn2slddq0jryjad")))))))

;; https://issues.guix.gnu.org/44165
(define-public emacs-xonsh-mode
  ;; There is no tagged release yet.
  (let ((commit "7fa581524533a9b6b770426e4445e571a69e469d")
        (revision "0"))
    (package
      (name "emacs-xonsh-mode")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/seanfarley/xonsh-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0lfi2372clkkzi4a940fwparsfhxxzb7bmysfd50n1myakgldri5"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/seanfarley/xonsh-mode")
      (synopsis "Emacs major mode for editing @code{xonshrc} files")
      (description
       "This package implements a major mode for xonsh scripts.  The basic
functionality includes syntax highlight for xonsh operators.  Files with the
@file{.xonshrc} or @file{.xsh} extension are automatically opened with this
mode.")
      (license license:gpl3+))))
