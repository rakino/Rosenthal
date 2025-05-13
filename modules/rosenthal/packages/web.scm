;; SPDX-FileCopyrightText: 2022, 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages web))

(define-public buku-run-dev
  (let ((revision "23")
        (commit "54fcdd77fc1e8e657b785b7d4ca8dc915e5f336b"))
    (package
      (inherit buku-run)
      (name "buku-run-dev")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/carnager/buku_run")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "079ygn39px71bypa54jn4z55iq24lxxcy7jv3ijy08iinqbfvldc")))))))

(define-public hugo
  (package
    (name "hugo")
    (version "0.147.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gohugoio/hugo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j0grh8sxd6ma9g406cbcwhwgfdazc4lg3r7jmiyrw2287d218yz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
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
             (method (go-mod-vendor #:go go-1.23))
             (uri (package-source this-package))
             (file-name "vendored-go-dependencies")
             (sha256
              (base32
               "1pwq7i0y2gb4cw9nriy699wa6pqlhz42rjkzv39g355nyszwpyj8")))
           (package-source libsass)
           (package-source libwebp)))
    (home-page "https://gohugo.io/")
    (synopsis "Static site generator written in Go")
    (description
     "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.  With its advanced templating system and fast asset
pipelines, Hugo renders a complete site in seconds, often less.")
    (license license:asl2.0)))

;; TODO: Package Forgejo without vendored dependencies.
(define-public forgejo
  (package
    (name "forgejo")
    (version "10.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://codeberg.org/forgejo/forgejo/releases/download/v"
                    version "/forgejo-src-" version ".tar.gz"))
              (sha256
               (base32
                "0cqp4x3xrvr7q1pkijqmf6jnx3wahi20xjfrv7ap81ykif83269x"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.23
           #:install-source? #f
           #:tests? #f                  ;TODO
           #:import-path "code.gitea.io/gitea"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X main.ReleaseVersion=" #$(package-version this-package)
                    " -X main.Version=" #$(package-version this-package)
                    " -X main.ForgejoVersion=" #$(package-version this-package)
                    " -X code.gitea.io/gitea/modules/setting.AppWorkPath=/var/lib/forgejo"
                    " -X code.gitea.io/gitea/modules/setting.CustomPath=" #$output "/etc/forgejo"
                    " -X code.gitea.io/gitea/modules/setting.CustomConf=/etc/forgejo/app.ini"))
           #:modules
           '(((guix build gnu-build-system) #:prefix gnu:)
             (guix build go-build-system)
             (guix build union)
             (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (assoc-ref gnu:%standard-phases 'unpack))
               (add-after 'unpack 'support-module
                 (lambda _
                   (unsetenv "GO111MODULE")
                   (substitute* "go.mod"
                     (("^toolchain.*") ""))))
               (replace 'build
                 (lambda* (#:key build-flags (parallel-build? #t)
                           #:allow-other-keys)
                   (let* ((njobs (if parallel-build? (parallel-job-count) 1)))
                     (setenv "GOMAXPROCS" (number->string njobs)))
                   (apply invoke "go" "install"
                          "-v" "-x"
                          "-ldflags=-s -w"
                          "-trimpath"
                          build-flags)))
               (replace 'install
                 (lambda _
                   (mkdir-p (in-vicinity #$output "/etc/forgejo"))
                   (copy-file
                    "custom/conf/app.example.ini"
                    (in-vicinity #$output "etc/forgejo/app.ini"))
                   (for-each
                    (lambda (dir)
                      (copy-recursively
                       dir (string-append #$output "/etc/forgejo/" dir)))
                    '("options" "public" "templates"))
                   (with-directory-excursion (in-vicinity #$output "bin")
                     (rename-file "gitea" "forgejo"))))
               (replace 'install-license-files
                 (assoc-ref gnu:%standard-phases 'install-license-files)))))
    (synopsis "Lightweight software forge")
    (description
     "Forgejo is a self-hosted lightweight software forge.  Easy to install and
low maintenance, it just does the job.")
    (home-page "https://forgejo.org/")
    (license license:gpl3+)))
