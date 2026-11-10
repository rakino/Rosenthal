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
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ai-robots-txt/ai.robots.txt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vfayd064mhksqyzak2qy08ydgqwfnf4jamzq25qlzqfi5fji9k4"))
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

(define-public caddy/dolly
  (package
    (inherit caddy)
    (name "caddy-dolly")
    (version "2026.10.23")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.guix.moe/hako/caddy.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1amgxdy9598jpm1zkp7yvrxhj3zyzlc8rzp64mivlhhv58gmnfmz"))))
    (build-system go-vendored-build-system)
    (arguments
     (substitute-keyword-arguments arguments
       ((#:go _ #f) go-1.27)
       ((#:import-path _ #f) ".")
       ((#:vendor-hash _ #f)
        (base32 "1k45xgbay9fyahh7ng8y6n58m9nnv2wyjqwzyjf3via08mlh1cjm"))))
    (native-inputs '())
    (inputs '())
    (propagated-inputs '())
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
    (license license:expat)
    (properties '((disable-updater? . #t)))))
