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
  (let ((revision "364")
        (commit "6884e3ad717419b4a64a5fab08c8cb9bd20a0b27"))
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
                  "1g3l6l1dzgq8v12fz9wczaz7izqd414fmdag481qi0ifcldyhi57")))))))

(define-public emacs-doom-modeline-dev
  (let ((base emacs-doom-modeline)
        (revision "33")
        (commit "d0cb6de14a712f3e0ccc479d8040251bfc3c87d1"))
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
                  "0ckmw618yv3gvxkzsm9w0wnnm4yg1q5ji6qzfhvr4p1vj82ns6sp"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (delete "emacs-all-the-icons" "emacs-dash"))))))

(define-public emacs-god-mode-dev
  (let ((revision "16")
        (commit "49c1a1753188e5b2788b8c1f1f9fbd1264460bab"))
    (package
      (inherit emacs-god-mode)
      (name "emacs-god-mode-dev")
      (version (git-version "2.17.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacsorphanage/god-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0h1kfvr4zahk8ihvri1r16b2nkg3dg3524ic64c6w0jing2gr37c")))))))

(define-public emacs-macrostep-geiser-dev
  (let ((revision "3")
        (commit "f6a2d5bb96ade4f23df557649af87ebd0cc45125"))
    (package
      (inherit emacs-macrostep-geiser)
      (name "emacs-macrostep-geiser-dev")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nbfalcon/macrostep-geiser")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dykvwcvg8n24z3fkx6rv3l1mhzmca4cxj0gsvvqsg9wp0az1fc7")))))))

(define-public emacs-rime-dev
  (let ((revision "10")
        (commit "74f3dff40e66c65388a9471eecbc1d9aca5612f7"))
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
                  "1bqhn6pvsgs1x882q7y5p372aify2jmyfmgmx99nzakxbfkb0yqp")))))))

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
