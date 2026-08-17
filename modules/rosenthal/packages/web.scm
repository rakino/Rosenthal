;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages web)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils cargo)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  ;; Guix build systems
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (rosenthal build-system go-vendored)
  ;; Guix packages
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

(define-public ai-robots-txt
  (package
    (name "ai-robots-txt")
    (version "1.50")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ai-robots-txt/ai.robots.txt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18ahcvhicbgn30km8aph1vq3l4x09aasj0xvz3b0s0p8pkqqmw7g"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "code"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           ''(("." "share/ai-robots-txt/"
               #:include ("robots.txt"
                          ".htaccess"
                          "nginx-block-ai-bots.conf"
                          "Caddyfile"
                          "haproxy-block-ai-bots.txt")))))
    (home-page "https://github.com/ai-robots-txt/ai.robots.txt")
    (synopsis "List of AI agents and robots to block")
    (description
     "This package provides a collection of configuration files to help
website owners block unwanted AI crawlers from accessing their sites.")
    (license license:expat)))

(define-public anubis-anti-crawler
  (package
    (name "anubis-anti-crawler")
    (version "1.27.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/TecharoHQ/anubis/releases/download/v"
                    version "/anubis-src-vendor-npm-" version ".tar.gz"))
              (sha256
               (base32
                "14c04mkgxrv8jmyl0i048mqgrrw9ibw7jzm9l64pch9hmssy4i7z"))))
    (build-system go-vendored-build-system)
    (arguments
     (list #:tests? (not (%current-target-system)) ;FIXME
           #:go go-1.26
           #:install-source? #f
           #:import-path "./cmd/anubis"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X github.com/TecharoHQ/anubis.Version="
                    #$(package-version this-package)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (let ((cmd (in-vicinity #$output "bin/anubis")))
                       (invoke cmd "--help")
                       (invoke cmd "--version"))))))))
    (home-page "https://anubis.techaro.lol/")
    (synopsis "Proof-of-work check to stop crawlers")
    (description
     "Anubis checks incoming HTTP requests using one or more challenges in
order to protect upstream resources from web crawlers.")
    (license license:expat)
    (properties '((upstream-name . "anubis")))))

(define-public caddy
  (package
    (name "caddy")
    (version "2.11.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/caddyserver/caddy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04ypcnmplbkmp7r5vi3q2lx270q2195c4h8ripdjc32z2qlkqff3"))))
    (build-system go-vendored-build-system)
    (arguments
     (list #:go go-1.26
           #:vendor-hash (base32 "0vjdmwp4qdxvkrn1h7pk9ckwi5cjksq7nbd96yqd2dqan4ri4v6q")
           #:tests? (not (%current-target-system)) ;TODO: Run test suite.
           #:install-source? #f
           #:import-path
           (if (string=? "caddy" (package-name this-package))
               "./cmd/caddy"
               ".")
           #:build-flags
           #~(list "-tags" "nobadger nomysql nopgx"
                   (string-append
                    "-ldflags="
                    " -X github.com/caddyserver/caddy/v2.CustomVersion="
                    #$(package-version this-package)))
           #:modules
           (cons '(ice-9 match)
                 %default-go-vendored-modules)
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-extras
                 (lambda _
                   (let ((caddy
                          (or (which "caddy")
                              (in-vicinity #$output "bin/caddy"))))
                     (invoke caddy "manpage" "--directory"
                             (in-vicinity #$output "share/man/man8"))
                     (map
                      (match-lambda
                        ((shell . path)
                         (let ((file (in-vicinity #$output path)))
                           (mkdir-p (dirname file))
                           (with-output-to-file file
                             (lambda ()
                               (invoke caddy "completion" shell))))))
                      '(("bash" . "etc/bash_completion.d/caddy")
                        ("fish" . "share/fish/vendor_completions.d/caddy.fish")
                        ("zsh"  . "share/zsh/site-functions/_caddy"))))))
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (let ((caddy (in-vicinity #$output "bin/caddy")))
                       (invoke caddy "help")
                       (invoke caddy "version"))))))))
    (home-page "https://caddyserver.com/")
    (synopsis "Extensible HTTP web server with automatic HTTPS")
    (description
     "Caddy is a web server designed for simplicity and ease of use.  It is
notable for its automatic HTTPS feature, which enables secure connections
without requiring complex configuration.  Caddy is built with a focus on
performance and flexibility, making it suitable for a variety of applications,
from serving static websites to running dynamic web applications.")
    (license license:asl2.0)))

(define-public caddy/dolly
  (package
    (inherit caddy)
    (name "caddy-dolly")
    (version "2026.06.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.guix.moe/hako/caddy.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b8606rbg57ylxz2q88335s04q8yvg6b33qkq3hk9895vwd74mq0"))))
    (arguments
     (substitute-keyword-arguments arguments
       ((#:vendor-hash _ #f)
        (base32 "0bwpayz2yf18cycffy02iiq88cl0smljimsf0yrdzrn1l3flg37s"))))
    (home-page "https://git.guix.moe/hako/caddy")
    (properties '((disable-updater? . #t)))))

(define-public forgejo
  (package
    (name "forgejo")
    (version "14.0.4")
    ;; TODO: Address npm dependencies and fetch from git.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://codeberg.org/forgejo/forgejo/releases/download/v"
                    version "/forgejo-src-" version ".tar.gz"))
              (sha256
               (base32
                "0mjmns7yqhhkqc3jcg8bffs64zj4b52hy2v1lvr14pq162r6wcil"))))
    (build-system go-vendored-build-system)
    (arguments
     (list #:tests? (not (%current-target-system)) ;TODO: Run test suite.
           #:go go-1.26
           #:install-source? #f
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X main.ReleaseVersion=" #$(package-version this-package)
                    " -X main.Version=" #$(package-version this-package)
                    " -X main.ForgejoVersion=" #$(package-version this-package)
                    " -X forgejo.org/modules/setting.AppWorkPath=/var/lib/forgejo"
                    " -X forgejo.org/modules/setting.CustomPath=" #$output "/etc/forgejo"
                    " -X forgejo.org/modules/setting.CustomConf=/etc/forgejo/app.ini"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'rename-binary
                 (lambda _
                   (rename-file (in-vicinity #$output "bin/forgejo.org")
                                (in-vicinity #$output "bin/forgejo"))))
               (add-after 'install 'install-extras
                 (lambda _
                   (mkdir-p (in-vicinity #$output "/etc/forgejo"))
                   (copy-file "custom/conf/app.example.ini"
                              (in-vicinity #$output "etc/forgejo/app.ini"))
                   (for-each
                    (lambda (dir)
                      (copy-recursively
                       dir (string-append #$output "/etc/forgejo/" dir)))
                    '("options" "public" "templates"))))
               (delete 'check)
               (add-after 'rename-binary 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (let ((forgejo (in-vicinity #$output "bin/forgejo")))
                       (invoke forgejo "--help")
                       (invoke forgejo "--version"))))))))
    (native-inputs (list git-minimal))
    (home-page "https://forgejo.org/")
    (synopsis "Lightweight software forge")
    (description
     "Forgejo is a self-hosted, lightweight software forge designed to
facilitate collaborative software development.  It is built to be easy to
install and maintain, making it an ideal choice for teams and organizations
looking for a reliable platform to manage their software projects.")
    (license license:gpl3+)
    (properties
     '((disable-updater? . #t)))))
