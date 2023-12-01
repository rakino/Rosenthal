;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages freedesktop)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python))

;; https://issues.guix.gnu.org/65525
;; Now in core-updates.
(define-public libinput-minimal-1.24.0
  (package
    (inherit libinput-minimal)
    (name "libinput-minimal")
    (version "1.24.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libinput/libinput.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xk0dljykjfmkks7kjxvbia6g3wadmy7lihfygm8icywkq8j0dw1"))))
    (native-inputs
     (modify-inputs (package-native-inputs libinput)
       (append python-minimal-wrapper python-pytest)))))
