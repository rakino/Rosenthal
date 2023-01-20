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
  #:use-module (gnu packages tls)
  #:use-module (rosenthal utils download))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define %cflags
  (string-append
   " -flto" " -fpic" " -fpie" " -fvisibility=hidden" " -fwrapv" " -pipe"
   " -fsanitize=cfi" " -fstack-clash-protection" " -fstack-protector-strong"
   " -enable-trivial-auto-var-init-zero-knowing-it-will-be-removed-from-clang"
   " -ftrivial-auto-var-init=zero -D_FORTIFY_SOURCE=2 -D_GLIBCXX_ASSERTIONS"))

(define %ldflags "-Wl,-z,defs -Wl,-z,now -Wl,-z,relro -Wl,-pie")

(define %xanmod-version "6.1.5")
(define %xanmod-revision "xanmod1")

(define %hardened-version "6.1.6")
(define %hardened-revision "hardened2")

(define linux-xanmod-patch
  (origin
    (method url-fetch/xz-file)
    (uri (string-append
          "https://github.com/xanmod/linux/releases/download/"
          %xanmod-version "-" %xanmod-revision "/patch-"
          %xanmod-version "-" %xanmod-revision ".xz"))
    (file-name
     (string-append "linux-" %xanmod-version "-" %xanmod-revision ".patch"))
    (sha256
     (base32 "0d3382f3vzrmsw366hd5k2dpzl9a8zhc1dq3bwg5yq82gi9ydyvl"))))

(define linux-hardened-patch
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/anthraxx/linux-hardened/releases/download/"
          %hardened-version "-" %hardened-revision "/linux-hardened-"
          %hardened-version "-" %hardened-revision ".patch"))
    (sha256
     (base32 "0g1khyg0pghq3kj6nn1gyvxy8as89bmc2ymibc64wwrq5jaqrgm6"))))

(define linux-xanmod-source
  (origin
    (inherit (%upstream-linux-source
              (version-major+minor %xanmod-version)
              (base32 "1ssxn81jfl0jf9brczfrrwd1f1vmf594jvhrs7zgcc54a5qg389c")))
    (file-name
     (string-append "linux-" %xanmod-version "-" %xanmod-revision ".tar.xz"))
    (patches (list linux-xanmod-patch))))

(define linux-hardened-source
  (origin
    (inherit (%upstream-linux-source
              %hardened-version
              (base32 "1qsygnsn67j843ywpswy5724zin5sszb5mz8b8h3lw553mb8wk9y")))
    (file-name
     (string-append "linux-" %hardened-version "-" %hardened-revision ".tar.xz"))
    (patches (list linux-hardened-patch))))

(define-public linux-xanmod
  (let ((base (customize-linux #:name "linux-xanmod"
                               #:linux linux-libre
                               #:source linux-xanmod-source
                               #:extra-version %hardened-revision)))
    (package
      (inherit base)
      (version %xanmod-version)
      (build-system
        (build-system-with-c-toolchain
         (package-build-system base)
         (modify-inputs (standard-packages)
           (delete "binutils" "gcc" "ld-wrapper"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'remove-localversion
                (lambda _
                  (when (file-exists? "localversion")
                    (delete-file "localversion"))))
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
                               #:extra-version %hardened-revision)))
    (package
      (inherit base)
      (version %hardened-version)
      (home-page "https://github.com/anthraxx/linux-hardened")
      (supported-systems '("aarch64-linux" "x86_64-linux"))
      (synopsis "The Security-Hardened Linux kernel and modules")
      (description
       "This package provides a Linux kernel with minimal supplement to
upstream Kernel Self Protection Project changes.  Features already provided by
SELinux + Yama and archs other than multiarch arm64 / x86_64 aren't in scope.
"))))

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
