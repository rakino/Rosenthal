;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages gnome-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public qogir-icon-theme
  (package
    (name "qogir-icon-theme")
    (version "2023.06.05")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vinceliuice/Qogir-icon-theme")
                    (commit (string-replace-substring version "." "-"))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet '(substitute* "install.sh"
                          (("gtk-update-icon-cache") "true")))
              (sha256
               (base32
                "1kn8b9zdamxbfbs7b9qpx53hmjw2l40sxpjw93axb1dqy81yc8da"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (let* ((dest (string-append #$output "/share/icons"))
                          (flags (list "--theme" "all"
                                       "--color" "all"
                                       "--dest" dest)))
                     (mkdir-p dest)
                     (apply invoke "sh" "install.sh" flags)))))))
    (home-page "https://www.pling.com/p/1296407/")
    (synopsis "Colorful design icon theme for linux desktops")
    (description "A flat colorful design icon theme for linux desktops.")
    (license license:gpl3+)))
