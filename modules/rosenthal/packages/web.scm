;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>

(define-module (rosenthal packages web)
  ;; Utilities
  #:use-module (guix deprecation)
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (rosenthal build-system go-vendored)
  ;; Guix packages
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages node)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

(define-public ai-robots-txt
  (package
    (name "ai-robots-txt")
    (version "2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ai-robots-txt/ai.robots.txt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bc1l5j63f4n35iygas8yl5j6s00s8lr8sa6jn8ms09ighq503ij"))
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

(define-deprecated/public-alias anubis-anti-crawler anubis-ai-firewall)

(define-public caddy
  (package
    (name "caddy")
    (version "2.11.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/caddyserver/caddy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gv0gjwdqd5azw5ws95am7jb653klyyl9k9sabb30bvs04zi5rgb"))))
    (build-system go-vendored-build-system)
    (arguments
     (list #:go go-1.27
           #:vendor-hash (base32 "06bcd41snksncmdijd32r3l2n5ch1v3gr4qkx45g2jb03zkab4wh")
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

;; https://codeberg.org/guix/guix/pulls/9006
(define-public forgejo
  (package
    (name "forgejo")
    (version "16.0.6")
    (source
     (origin
       (method url-fetch)
       ;; XXX: We build from the release tarball for now, which includes all
       ;; of the needed Go dependencies.
       ;; TODO: "Unbundle" the index.js and index.css by building them with
       ;; webpack.  This cannot currently be done because of missing Node
       ;; modules.
       (uri (string-append "https://codeberg.org/forgejo/forgejo/"
                           "releases/download/v" version
                           "/forgejo-src-" version ".tar.gz"))
       (sha256
        (base32
         "0c56zxadj8rpn9xj7i3l97z2xh0w318vwrbxmb1nw071qh5krs8g"))))
    (build-system gnu-build-system)
    (outputs (list "out" "data"))
    (arguments
     (list
      #:test-target "test-backend"      ;test-frontend requires npm modules
      #:make-flags #~(list "TAGS=sqlite sqlite_unlock_notify") ;sqlite support
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "modules/git/hook_generate.go"
                (("/usr/bin/env")
                 (search-input-file inputs "bin/env")))))
          (add-after 'unpack 'set-static-root-path
            (lambda _
              (substitute* "modules/setting/server.go"
                (("StaticRootPath = AppWorkPath")
                 (format #f "StaticRootPath = ~s" #$output:data)))))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (for-each
               delete-file
               ;; Theses tests require internet.
               '("modules/hcaptcha/hcaptcha_test.go"
                 "modules/updatechecker/update_checker_test.go"
                 "services/migrations/migrate_test.go"
                 "services/migrations/allowlist/is_migrate_allowed_test.go"
                 ;; The 'TestGetDirectorySize' test expects 8037
                 ;; entries, finds 7597 for unknown reasons.
                 "modules/repository/create_test.go"))))
          (replace 'configure
            (lambda _
              (setenv "CC" "gcc")
              (setenv "CGO_ENABLED" "1")
              (setenv "GOTOOLCHAIN" "local") ;use 'go' from Guix
              (setenv "HOME" (getcwd))
              (invoke "git" "init")))   ;for tests
          (add-after 'install 'post-install
            (lambda* (#:key inputs #:allow-other-keys)
              (rename-file "go/bin/forgejo.org" "go/bin/forgejo")
              (let ((forgejo (string-append #$output "/bin/forgejo")))
                (install-file "go/bin/forgejo" (dirname forgejo))
                (wrap-program forgejo
                  `("PATH" prefix
                    ,(map (lambda (c)
                            (dirname
                             (search-input-file inputs
                                                (string-append "bin/" c))))
                          '("git" "gzip" "sh" "ssh"))))
                (substitute* forgejo    ;avoid stripping argv[0] in wrapper
                  (("exec -a \"\\$\\{0##\\*/\\}\" ")
                   "exec ")))
              ;; Install data files.
              (copy-recursively "options"
                                (string-append #$output:data "/options"))
              (copy-recursively "public"
                                (string-append #$output:data "/public"))
              (copy-recursively "templates"
                                (string-append #$output:data "/templates")))))))
    (native-inputs (list git go-1.26 openssh node sqlite))
    (inputs (list bash coreutils git gzip openssh))
    (home-page "https://forgejo.org/")
    (synopsis "Self-hosted, lightweight software forge")
    (description
     "Forgejo is a lightweight software forge that aims to be easy to self-host
and maintain.  It offers a rich web API for accessing its data.  ALong the usual
forge features such as issues and pull requests tracking, it also includes
simple software project management features like roles, scopes, and time
tracking to ease collaboration.")
    (license license:expat)))
