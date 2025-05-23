;; SPDX-FileCopyrightText: 2022, 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages web)
  #:use-module (gnu packages version-control)
  #:use-module (rosenthal packages golang))

(define-public ai-robots-txt
  (package
    (name "ai-robots-txt")
    (version "1.31")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ai-robots-txt/ai.robots.txt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bvq24w8pq56knhdacjkq93v6l719jcj3jf4fsknlmp9m6izm3zj"))
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

(define-public caddy
  (package
    (name "caddy")
    (version "2.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/caddyserver/caddy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00crxr956sp865pc3mg0zsacsy80s8v4jgqpmbq3hrsk2gcdsc47"))
              (modules '((guix build utils)))
              (snippet '(substitute* "go.mod"
                          (("^toolchain.*") "")))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.24
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
           '((ice-9 match)
             ((guix build gnu-build-system) #:prefix gnu:)
             (guix build go-build-system)
             (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda args
                   (unsetenv "GO111MODULE")
                   (apply (assoc-ref gnu:%standard-phases 'unpack) args)
                   (copy-recursively
                    #+(this-package-native-input "vendored-go-dependencies")
                    "vendor")))
               (replace 'install-license-files
                 (assoc-ref gnu:%standard-phases 'install-license-files))
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
    (native-inputs
     (list (origin
             (method (go-mod-vendor #:go go-1.24))
             (uri (package-source this-package))
             (file-name "vendored-go-dependencies")
             (sha256
              (base32
               "0iwxhc85xnhpqrahiaw1017vxg27hc5q22rc0f96g42mc2mbi2zl")))))
    (home-page "https://caddyserver.com/")
    (synopsis "Extensible HTTP web server with automatic HTTPS")
    (description
     "Caddy is a web server designed for simplicity and ease of use.  It is
notable for its automatic HTTPS feature, which enables secure connections
without requiring complex configuration.  Caddy is built with a focus on
performance and flexibility, making it suitable for a variety of applications,
from serving static websites to running dynamic web applications.")
    (license license:asl2.0)))

(define-public caddy/hako
  (package
    (inherit caddy)
    (name "caddy-hako")
    (version "2025.05.23-1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.boiledscript.com/hako/caddy.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03jdlyakaxysz5wmxbwrdwl3rgkp0a0ws10g27hj1lmkdic860h9"))))
    (native-inputs
     (modify-inputs (package-native-inputs caddy)
       (replace "vendored-go-dependencies"
         (origin
           (method (go-mod-vendor #:go go-1.24))
           (uri (package-source this-package))
           (file-name "vendored-go-dependencies")
           (sha256
            (base32
             "0m01p9y96m1krjg1rf53kndxklql8i4hfv09rc3xnxbmqh5ahm43"))))))
    (home-page "https://git.boiledscript.com/hako/caddy")))

(define-public hugo
  (package
    (name "hugo")
    (version "0.147.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gohugoio/hugo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jn40dh4rh35lmiwbyrpdrdlrn8g31xzbm73wdhns2av4d35mg9s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.24
      #:install-source? #f
      #:import-path "."
      #:build-flags
      #~(list "-tags" "extended withdeploy"
              (string-append
               "-ldflags="
               " -X github.com/gohugoio/hugo/common/hugo.vendorInfo=Nonguix"))
      #:test-flags ''("-skip=^TestCommands/mod|^TestCommands/server")
      #:test-subdirs ''(".")
      #:modules
      '(((guix build gnu-build-system) #:prefix gnu:)
        (guix build go-build-system)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda args
              (unsetenv "GO111MODULE")
              (apply (assoc-ref gnu:%standard-phases 'unpack) args)
              (copy-recursively
               #+(this-package-native-input "vendored-go-dependencies")
               "vendor")))
          (replace 'install-license-files
            (assoc-ref gnu:%standard-phases 'install-license-files))
          (add-after 'unpack 'fix-paths
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (setenv "C_INCLUDE_PATH"
                      (string-append
                       (getenv "C_INCLUDE_PATH") ":"
                       (dirname
                        (dirname
                         (dirname
                          (search-input-file
                           (or native-inputs inputs)
                           "src/dec/alphai_dec.h"))))))
              (with-directory-excursion "vendor/github.com/bep/gowebp"
                (substitute* (find-files "internal/libwebp")
                  (("../../libwebp_src/(.*)\"" _ file)
                   (format #f "~a\""
                           (search-input-file
                            (or native-inputs inputs) file)))))
              (with-directory-excursion "vendor/github.com/bep/golibsass"
                (substitute* (find-files "internal/libsass")
                  (("../../libsass_src/(.*)\"" _ file)
                   (format #f "~a\""
                           (search-input-file
                            (or native-inputs inputs) file))))))))))
    (native-inputs
     (list (origin
             (method (go-mod-vendor #:go go-1.24))
             (uri (package-source this-package))
             (file-name "vendored-go-dependencies")
             (sha256
              (base32
               "0wrzl37mv2qcww6s2bk4jf4gjj6pkxnwm44q8qrg0vijch8pxdbq")))
           (package-source libsass)
           (package-source libwebp)))
    (home-page "https://gohugo.io/")
    (synopsis "Static site generator written in Go")
    (description
     "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.  With its advanced templating system and fast asset
pipelines, Hugo renders a complete site in seconds, often less.")
    (license license:asl2.0)))

(define-public forgejo
  (package
    (name "forgejo")
    (version "11.0.1")
    ;; TODO: Address npm dependencies and fetch from git.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://codeberg.org/forgejo/forgejo/releases/download/v"
                    version "/forgejo-src-" version ".tar.gz"))
              (sha256
               (base32
                "1mpdbwq3h0l5yk8f2sjpnyr0b6wngryx3238166rf7l2k5869bmq"))
              (modules '((guix build utils)))
              ;; Avoid downloading toolchain.
              (snippet '(substitute* "go.mod"
                          (("^toolchain.*") "")))))
    (build-system go-build-system)
    (arguments
     (list #:tests? (not (%current-target-system)) ;TODO: Run test suite.
           #:go go-1.24
           #:install-source? #f
           #:import-path "."
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X main.ReleaseVersion=" #$(package-version this-package)
                    " -X main.Version=" #$(package-version this-package)
                    " -X main.ForgejoVersion=" #$(package-version this-package)
                    " -X forgejo.org/modules/setting.AppWorkPath=/var/lib/forgejo"
                    " -X forgejo.org/modules/setting.CustomPath=" #$output "/etc/forgejo"
                    " -X forgejo.org/modules/setting.CustomConf=/etc/forgejo/app.ini"))
           #:modules
           '(((guix build gnu-build-system) #:prefix gnu:)
             (guix build go-build-system)
             (guix build union)
             (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda args
                   (unsetenv "GO111MODULE")
                   (apply (assoc-ref gnu:%standard-phases 'unpack) args)))
               (replace 'install-license-files
                 (assoc-ref gnu:%standard-phases 'install-license-files))
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
     '((rosenthal-update? . #f)))))
