;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs-xyz))

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
