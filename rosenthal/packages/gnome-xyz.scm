;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages gnome-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 string-fun)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public qogir-icon-theme
  (package
    (name "qogir-icon-theme")
    (version "2022.11.05")
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
                "1wcqa0wla6qlyfv3g5zhailpl0lyfck8fybxih8ka9bd2fdws399"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:configure-flags
           #~(list "--dest" (string-append #$output "/share/icons")
                   "--theme" "all"
                   "--color" "all")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'bootstrap)
               (delete 'configure)
               (delete 'build)
               (replace 'install
                 (lambda* (#:key (configure-flags '()) #:allow-other-keys)
                   (mkdir-p
                    (cadr (or (member "--dest" configure-flags)
                              (member "-d" configure-flags))))
                   (apply invoke "sh" "install.sh" configure-flags))))))
    (home-page "https://www.pling.com/p/1296407/")
    (synopsis "Colorful design icon theme for linux desktops")
    (description "A flat colorful design icon theme for linux desktops.")
    (license license:gpl3)))
