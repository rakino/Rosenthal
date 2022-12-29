;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages tls))

(define computed-origin-method
  (@@ (guix packages) computed-origin-method))

(define deblob-scripts
  (@@ (gnu packages linux) deblob-scripts-6.0))

(define make-linux-libre-source
  (@@ (gnu packages linux) make-linux-libre-source))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define linux-rosenthal-deblob-scripts
  (match deblob-scripts
    ((deblob-version (? origin? deblob) (? origin? deblob-check))
     (list deblob-version
           (origin
             (inherit deblob)
             (file-name "linux-libre-deblob")
             (patches (list (local-file "patches/linux-libre-deblob-keep-needed.patch"))))
           (origin
             (inherit deblob-check)
             (file-name "linux-libre-deblob-check")
             (patches (list (local-file "patches/linux-libre-deblob-check-omit-error.patch"))))))))

(define %cflags
  (string-append
   " -flto" " -fpic" " -fpie" " -fvisibility=hidden" " -fwrapv" " -pipe"
   " -fsanitize=cfi" " -fstack-clash-protection" " -fstack-protector-strong"
   " -enable-trivial-auto-var-init-zero-knowing-it-will-be-removed-from-clang"
   " -ftrivial-auto-var-init=zero -D_FORTIFY_SOURCE=2 -D_GLIBCXX_ASSERTIONS"))

(define %ldflags "-Wl,-z,defs -Wl,-z,now -Wl,-z,relro -Wl,-pie")

(define %linux-version "6.0.12")
(define %xanmod-version "xanmod1")
(define %hardened-version "hardened1")

(define (extract-xanmod-patch version hash)
  (let ((patch (string-append "linux-" version ".patch"))
        (source (origin
                  (method url-fetch)
                  (uri (string-append "https://github.com/xanmod/linux"
                                      "/releases/download/" version
                                      "/patch-" version ".xz"))
                  (sha256 hash))))
    (origin
      (method computed-origin-method)
      (file-name patch)
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules '((guix build utils))
           #~(begin (use-modules (guix build utils))
                    (set-path-environment-variable
                     "PATH" '("bin") (list #+xz))
                    (setenv "XZ_OPT" (string-join (%xz-parallel-args)))
                    (map (lambda (p)
                           (begin
                             (copy-file #+source p)
                             (make-file-writable p)
                             (invoke "xz" "--decompress" p)))
                         (list (string-append #$patch ".xz")))
                    (copy-file #$patch #$output))))))))

(define linux-xanmod-patch
  (extract-xanmod-patch
   (string-append %linux-version "-" %xanmod-version)
   (base32 "05lm7prllbsgbfpw9v41idyd7a4xyypxry8ncsmpdzlak602rj7k")))

(define linux-hardened-patch
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/anthraxx/linux-hardened/releases/download/"
          %linux-version "-" %hardened-version "/linux-hardened-"
          %linux-version "-" %hardened-version ".patch"))
    (sha256
     (base32 "1fhpx9hx2csz8d30l2nl43j865k6gzf5fim0943dhgpa3r9cy83w"))))

(define linux-hardened-patch-for-xanmod
  (origin
    (inherit linux-hardened-patch)
    (file-name "linux-hardened.patch")
    (patches
     (list (local-file "patches/linux-hardened-xanmod-adaption.patch")))))

(define linux-xanmod-source
  (origin
    (inherit (%upstream-linux-source
              "6.0"
              (base32 "13kqh7yhifwz5dmd3ky0b3mzbh9r0nmjfp5mxy42drcdafjl692w")))
    (patches (list linux-xanmod-patch))))

(define linux-hardened-source
  (origin
    (inherit (%upstream-linux-source
              %linux-version
              (base32 "00ag63lnxw2gijw3b6v29lhrlv480m12954q5zh4jawlz3nk1dw9")))
    (patches (list linux-hardened-patch))))

(define linux-rosenthal-source
  (origin
    (inherit linux-xanmod-source)
    (patches
     (list linux-xanmod-patch
           linux-hardened-patch-for-xanmod))))

(define linux-rosenthal-source-deblobed
  (make-linux-libre-source
   %linux-version
   linux-rosenthal-source
   linux-rosenthal-deblob-scripts))

(define-public linux-xanmod
  (let ((base (customize-linux #:name "linux-xanmod"
                               #:linux linux-libre
                               #:source linux-xanmod-source
                               #:extra-version %xanmod-version)))
    (package
      (inherit base)
      (version %linux-version)
      (build-system
        (build-system-with-c-toolchain
         (package-build-system base)
         (modify-inputs (standard-packages)
           (delete "binutils" "gcc" "ld-wrapper"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-before 'configure 'setenv
                (lambda _
                  (setenv "LLVM" "1")
                  (setenv "CFLAGS" #$%cflags)
                  (setenv "CXXFLAGS" #$%cflags)
                  (setenv "LDFLAGS" #$%ldflags)

                  ;; FIXME:
                  ;; libgcc_s.so.1 must be installed for pthread_cancel to work
                  ;; scripts/link-vmlinux.sh: line 189: 22065 Aborted                 (core dumped) ${objtree}/scripts/sorttable ${1}
                  ;; Failed to sort kernel tables
                  (setenv "LD_PRELOAD"
                          (string-append #$gcc:lib "/lib/libgcc_s.so.1"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append clang-15
                 llvm-15
                 lld-as-ld-wrapper-15
                 python-minimal-wrapper
                 zstd)
         (delete "gmp" "mpc" "mpfr")))
      (home-page "https://xanmod.org/")
      (supported-systems '("x86_64-linux"))
      (synopsis
       "Linux kernel distribution with custom settings and new features")
      (description
       "General-purpose Linux kernel distribution with custom settings and new
features.  Built to provide a stable, responsive and smooth desktop
experience."))))

(define-public linux-hardened
  (let ((base (customize-linux #:name "linux-hardened"
                               #:linux linux-xanmod
                               #:source linux-hardened-source
                               #:extra-version %hardened-version)))
    (package
      (inherit base)
      (home-page "https://github.com/anthraxx/linux-hardened")
      (supported-systems '("aarch64-linux" "x86_64-linux"))
      (synopsis "The Security-Hardened Linux kernel and modules")
      (description
       "This package provides a Linux kernel with minimal supplement to
upstream Kernel Self Protection Project changes.  Features already provided by
SELinux + Yama and archs other than multiarch arm64 / x86_64 aren't in scope.
"))))

(define-public linux-rosenthal
  (let ((base (customize-linux #:name "linux-rosenthal"
                               #:linux linux-xanmod
                               #:source linux-rosenthal-source-deblobed
                               #:defconfig (local-file "aux-files/config.zen3-dorphine")
                               #:extra-version "rosenthal")))
    (package
      (inherit base)
      (home-page "https://github.com/rakino/rosenthal/")
      (supported-systems '("x86_64-linux"))
      (synopsis "Custom Linux kernel")
      (description
       "Linux-Rosenthal is a custom Linux kernel based on @code{linux-xanmod}
and @code{linux-hardened}.  This kernel is partially deblobed, with some files
necessary to drive specific hardwares kept."))))

(define-public kconfig-hardened-check-dev
  (let* ((base kconfig-hardened-check)
         (revision "154")
         (commit "6211b6852b6b35f6f5d18ec2f0e713d2afea5a87"))
    (package
      (inherit base)
      (name "kconfig-hardened-check-dev")
      (version (git-version "0.5.17" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/a13xp0p0v/kconfig-hardened-check")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ynd4s0dm5aqhk6y858p9iwl1c0y5mp5rb2a4mdgkzc23v5aczyi")))))))
