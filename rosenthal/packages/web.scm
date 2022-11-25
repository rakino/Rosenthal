;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages web)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59558
(define-public buku-run
  (package
    (name "buku-run")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/carnager/buku_run")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zyjjf3b8g3dnymcrg683rbnc6qrvx8ravfm833n7kjrqky3bczn"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:make-flags
           #~(list (string-append "DESTDIR=" #$output)
                   "PREFIX=")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'fixpath
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "buku_run"
                     ((" \\<(rofi)\\>" all cmd)
                      (string-append " " (search-input-file inputs "/bin/rofi")))
                     (("\\<(buku)\\> " all cmd)
                      (string-append (search-input-file inputs "/bin/buku") " "))
                     (("\\<(awk|gawk)\\>" cmd)
                      (search-input-file inputs "/bin/awk"))
                     (("/etc/buku_run.config" path)
                      (string-append #$output path))))))))
    (inputs (list buku rofi))
    (home-page "https://github.com/carnager/buku_run")
    (synopsis "Rofi frontend for buku bookmarks manager")
    (description
     "This package provides a @code{rofi} frontend for @code{buku} bookmark
manager.")
    (license license:gpl3)))
