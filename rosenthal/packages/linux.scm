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
  #:use-module (gnu packages cpio)
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

(define source-with-patches
  (@@ (gnu packages linux) source-with-patches))

(define %default-extra-linux-options
  (@@ (gnu packages linux) %default-extra-linux-options))

(define config->string
  (@@ (gnu packages linux) config->string))

(define %xanmod-version "6.1.7")
(define %xanmod-revision "xanmod1")

(define %hardened-version "6.1.7")
(define %hardened-revision "hardened1")

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
     (base32 "1k5n4cli7r2w66132yh46gw6n0fk10p7kxgknldf2ydfws1q8rs8"))))

(define linux-hardened-patch
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/anthraxx/linux-hardened/releases/download/"
          %hardened-version "-" %hardened-revision "/linux-hardened-"
          %hardened-version "-" %hardened-revision ".patch"))
    (sha256
     (base32 "1hp3mbl8vfd2cwpxbhmqqy77nzyk265k1rcf1rz048ivnsppw4cx"))))

(define linux-xanmod-source
  (source-with-patches (%upstream-linux-source
                        (version-major+minor %xanmod-version)
                        (base32
                         "1ssxn81jfl0jf9brczfrrwd1f1vmf594jvhrs7zgcc54a5qg389c"))
                       (list linux-xanmod-patch)))

(define linux-hardened-source
  (source-with-patches (%upstream-linux-source
                        %hardened-version
                        (base32
                         "03v0pvg831qzbpc09ip1h0p4zz6js9das7vzh8xhsf77sax4ic2a"))
                       (list linux-hardened-patch)))

(define-public linux-xanmod
  (let ((base (customize-linux #:name "linux-xanmod"
                               #:source linux-xanmod-source
                               #:defconfig "config_x86-64-v1"
                               #:extra-version %xanmod-revision)))
    (package
      (inherit base)
      (version %xanmod-version)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'remove-localversion
                (lambda _
                  (when (file-exists? "localversion")
                    (delete-file "localversion"))))
              (add-before 'configure 'add-defconfig
                (lambda _
                  (copy-file "CONFIGS/xanmod/gcc/config_x86-64-v1" ".config")

                  ;; Adapted from `make-linux-libre*'.
                  (chmod ".config" #o666)
                  (let ((port (open-file ".config" "a"))
                        (extra-configuration #$(config->string
                                                ;; Guix kernels have NVMe
                                                ;; support built-in.

                                                ;; FIXME: There might be other
                                                ;; support missing.
                                                (cons* '("CONFIG_BLK_DEV_NVME" . #t)
                                                       %default-extra-linux-options))))
                    (display extra-configuration port)
                    (close-port port))
                  (invoke "make" "oldconfig")

                  (rename-file ".config" "arch/x86/configs/config_x86-64-v1")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         ;; cpio is needed for CONFIG_IKHEADERS.
         (append cpio zstd)))
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
