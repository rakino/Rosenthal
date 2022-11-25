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
  (let ((revision "357")
        (commit "48fea7a905b3bcc6d97609316beced666da89b1f"))
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
                  "17w9irn5aaxadsm5smz5cm1dxy0xb1hh0dnhwqclq3y5llrb21lx")))))))

(define-public emacs-doom-modeline-dev
  (let ((base emacs-doom-modeline)
        (revision "26")
        (commit "440fd160b4dff530465938bed40094a0d413fc11"))
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
                  "0ablv6fyqb6mzzk7iyx24nj5xmmmhw7kqmi2xldjv0a7aplmxbqr"))))
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
