;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022-2024 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages emacs-xyz)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system emacs)
  ;; Guix packages
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pdf)
  #:use-module (rosenthal packages version-control))

(define-public emacs-caddyfile-mode
  (let ((commit "fc41148f5a7eb320f070666f046fb9d88cf17680")
        (revision "0"))
    (package
      (name "emacs-caddyfile-mode")
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Schnouki/caddyfile-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1s9kbav5wbyividn9zncd153h89nil0i9aj9hgxa95q9fy84r23w"))))
      (build-system emacs-build-system)
      ;; No tests.
      (arguments (list #:tests? #f))
      (propagated-inputs (list emacs-loop))
      (home-page "https://github.com/Schnouki/caddyfile-mode")
      (synopsis "Emacs major mode for editing Caddy configuration files")
      (description
       "This package provides @code{caddyfile-mode}, an Emacs major mode for
editing Caddyfiles, configuration files for @code{caddy}.")
      (license license:gpl3+)
      (properties
       '((disable-updater? . #t))))))

(define-public emacs-eat/dolly
  (package
    (inherit
     (package-with-extra-patches emacs-eat
       (list (origin
               (method url-fetch)
               (uri "https://codeberg.org/akib/emacs-eat/pulls/133.patch")
               (sha256
                (base32
                 "0rn2xf2jq9i42ndcd3wxrr5lid8r4kxqiyr84dasv5ihqg8as5d5")))
             (origin
               (method url-fetch)
               (uri "https://codeberg.org/akib/emacs-eat/pulls/228.patch")
               (sha256
                (base32
                 "14d6nvagcri0r687y3m179ijmnad89m5d5dj2rn2p6rpfc5nssr0"))))))
    (name "emacs-eat-dolly")))

(define-public emacs-flycheck-eglot
  (let ((commit "cd1dd78cec0ae1f566c765d98bbff322cc7b67ef")
        (revision "0"))
    (package
      (name "emacs-flycheck-eglot")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/flycheck/flycheck-eglot")
                (commit commit)))
         (sha256 (base32 "19i2a33mpddd64mnvjk247ayn325p66lknm9pqjb0ccjfwi54sml"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-flycheck))
      (home-page "https://github.com/flycheck/flycheck-eglot")
      (synopsis "Flycheck support for eglot")
      (description
       "This package provides a simple \"glue\" minor mode that allows Flycheck
and Eglot to work together.  Thus, the Flycheck frontend can display the results
of syntactic checks performed by the LSP server.")
      (license (list license:gpl3+ license:expat))
      (properties '((disable-updater? . #t))))))

(define-public emacs-isearch-mb
  (package
    (name "emacs-isearch-mb")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/isearch-mb-"
                           version ".tar"))
       (sha256
        (base32 "1b4929vr5gib406p51zcvq1ysmzvnz6bs1lqwjp517kzp6r4gc5y"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/astoff/isearch-mb")
    (synopsis "Control @code{isearch} from the minibuffer")
    (description
     "This Emacs package provides an alternative @code{isearch} UI based on the
minibuffer.  This allows editing the search string in arbitrary ways without any
special maneuver.  Unlike standard @code{isearch}, cursor motion commands do not
end the search.  Moreover, the search status information in the echo area and
some keybindings are slightly simplified.")
    (license license:gpl3+)
    (properties '((disable-updater? . #t)))))

(define-public emacs-kdl-mode
  (let ((commit "2d849e298199f490e4894c01764a8a83decd704a")
        (revision "0"))
    (package
      (name "emacs-kdl-mode")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/taquangtrung/emacs-kdl-mode")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18va5ipay9grbv84kwq9ji4dn1sabnzydj4hb1mjd9mbbibw1wg4"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))    ;No tests.
      (home-page "https://github.com/taquangtrung/emacs-kdl-mode")
      (synopsis "Emacs major mode for editing files in the KDL document language")
      (description "")
      (license license:gpl3+)
      (properties '((disable-updater? . #t))))))

(define-public emacs-magit-delta/dolly
  (package
    (inherit
     (package-with-extra-patches emacs-magit-delta
       (list (origin
               (method url-fetch)
               (uri "https://github.com/dandavison/magit-delta/pull/34.patch")
               (sha256
                (base32
                 "1ipqvrszh6fp9d9g3grk4mbh762knhn0zp3jhww0mkib73f3p0sy"))))))
    (name "emacs-magit-delta-dolly")
    (properties '((disable-updater? . #t)))))

(define-public emacs-majutsu/dolly
  (let ((commit "9c2fdb31b0b16eb211afc3e3e342fa125d3aa0a6")
        (revision "4"))
    (package
      (inherit emacs-majutsu)
      (name "emacs-majutsu-dolly")
      (version (git-version "0.6.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/0WD0/majutsu")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0a8kdziz6lq6jqw4a4qbiq7alxll273j5piqf09jcw1irc5gg2xm"))))
      (arguments
       (substitute-keyword-arguments arguments
         ((#:tests? _ #t) #f)))
      (propagated-inputs
       (modify-inputs propagated-inputs
         (prepend emacs-consult emacs-plz)))
      (properties '((disable-updater? . #t))))))

(define-public emacs-pcmpl-tailscale
  (let ((commit "acd6bce54af506b0450cf6aa1068f63d4e25c8ce")
        (revision "0"))
    (package
      (name "emacs-pcmpl-tailscale")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.thanosapollo.org/pcmpl-tailscale")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lk808ahy8ckg2fr2pqk3p5if81nqrwsajrgqafv9hgn8w4l1x0p"))))
      (build-system emacs-build-system)
      (home-page "https://git.thanosapollo.org/pcmpl-tailscale")
      (synopsis "Enhanced shell completions for tailscale")
      (description
       "This package provides enhanced completions for the tailscale command
and it's subcommands.")
      (license license:gpl3+)
      (properties
       '((disable-updater? . #t))))))

(define-public emacs-reader
  (let ((commit "87b193d6996093530ab01cb0314c4d23b9777057")
        (revision "1"))
    (package
      (name "emacs-reader")
      (version (git-version "0.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://codeberg.org/divyaranjan/emacs-reader")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0s9hs1rmgqa68mpjv8lq754sfbl04wrmpkkclwl4ljy8xra9r6by"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:tests? #f ;no tests
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'build-module
              (lambda _
                (invoke "make" "USE_PKGCONFIG=no")))
            (add-after 'build-module 'install-module
              (lambda _
                (install-file "render-core.so" (elpa-directory #$output)))))))
      (inputs (list mupdf))
      (home-page "https://codeberg.org/divyaranjan/emacs-reader")
      (synopsis "Emacs document reader")
      (description
       "GNU Emacs document reader that supports all major document formats.")
      (license license:gpl3+)
      (properties '((disable-updater? . #t))))))

;; https://issues.guix.gnu.org/59552
(define-public emacs-wakatime-mode
  ;; No release since May 5, 2015.
  (let ((commit "1c5b2254dd72f2ff504d6a6189a8c10be03a98d1")
        (revision "60"))
    (package
      (name "emacs-wakatime-mode")
      ;; 1.0.2 on commit 32a0154cd4bbd525d354997e6b12c6a9726d0b43, not tagged
      (version (git-version "1.0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wakatime/wakatime-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "00qv6b756qiaqrmfg1w03psnsdj0iaz3sp50ib4kmdm2g9vgxl1s"))))
      (build-system emacs-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 ;; XXX: WakaTime hasn't packaged in Guix yet.
                 (delete 'patch-el-files))))
      (home-page "https://wakatime.com/emacs")
      (synopsis "Automatic time tracking extension for Emacs using WakaTime")
      (description
       "WakaTime mode is an Emacs minor mode for automatic time tracking and
metrics generated from your programming activity.")
      (license license:gpl3+)
      (properties
       '((disable-updater? . #t))))))
