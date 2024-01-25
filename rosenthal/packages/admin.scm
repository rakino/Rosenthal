;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages admin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public libseat-sans-logind
  (let ((base libseat))
    (package
      (inherit base)
      (name "libseat-sans-logind")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags configure-flags)
          #~(append #$configure-flags
                    (list "-Dlibseat-logind=disabled")))))
      (propagated-inputs '()))))

(define-public seatd-sans-logind
  (let ((base seatd))
    (package
      (inherit base)
      (name "seatd-sans-logind")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags configure-flags)
          #~(append #$configure-flags
                    (list "-Dlibseat-logind=disabled")))))
      (propagated-inputs '()))))

(define-public pam-uaccess
  (let ((commit "54fbf043c63cc500b4850b0b4a12ea14078f2b53")
        (revision "0"))
    (package
      (name "pam-uaccess")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~kennylevinsen/pam_uaccess")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "08068cw4nvcanym8b5dyccnnb3qc3f09pbvi6fcfiz227yx73npc"))))
      (build-system meson-build-system)
      (native-inputs (list pkg-config))
      (inputs (list acl eudev linux-pam))
      (home-page "https://git.sr.ht/~kennylevinsen/pam_uaccess")
      (synopsis
       "PAM module that grants access to devices tagged @code{uaccess} in udev")
      (description
       "@code{pam_uaccess} is a PAM module that grants access to devices tagged
@code{uaccess} in udev for the duration of the users' session, replacing
elogind's uaccess feature.")
      (license license:expat))))
