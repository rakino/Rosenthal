;; SPDX-FileCopyrightText: 2022-2024 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public emacs-nftables-mode
  (package
    (name "emacs-nftables-mode")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/nftables-mode-"
                           version ".tar"))
       (sha256
        (base32 "1wjw6n60kj84j8gj62mr6s97xd0aqvr4v7npyxwmhckw9z13xcqv"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/nftables-mode.html")
    (synopsis "Major mode for editing nftables scripts")
    (description
     "@code{nftables-mode} is an Emacs major mode for editing nftables scripts.
It currently only offers basic highlighting and primitive indentation.")
    (license license:gpl3+)))

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
      (license license:gpl3+))))

(define-public emacs-treesit-auto
  (package
    (name "emacs-treesit-auto")
    ;; NOTE: Not tagged, also change commit when updating.
    (version "1.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/renzmann/treesit-auto")
             (commit "016bd286a1ba4628f833a626f8b9d497882ecdf3")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03bvam7cpxqp4idhd235n76qdqhsbgw7m2lphy8qqwslbmcq23m4"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/renzmann/treesit-auto")
    (synopsis "Automatically use tree-sitter major modes")
    (description
     "@code{treesit-auto} is an Emacs package for automatically using tree-sitter
major modes and falling back to the original major mode when its tree-sitter
counterpart is unavailable.")
    (license license:gpl3+)))

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
      (license license:gpl3+))))
