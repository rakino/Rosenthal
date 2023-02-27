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
  #:use-module (gnu packages tls))

(define %default-extra-linux-options
  (@@ (gnu packages linux) %default-extra-linux-options))

(define config->string
  (@@ (gnu packages linux) config->string))

(define %xanmod-version "6.2.1")
(define %xanmod-revision "xanmod1")

(define %hardened-version "6.1.14")
(define %hardened-revision "hardened1")

(define linux-xanmod-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/xanmod/linux/archive/"
                        %xanmod-version "-" %xanmod-revision ".tar.gz"))
    (sha256
     (base32 "1bpy666ifxw79rjc3bxnngikdgcqq0qz1xaimgflmnwk80nydifl"))))

(define linux-hardened-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/anthraxx/linux-hardened/archive/"
                        %hardened-version "-" %hardened-revision ".tar.gz"))
    (sha256
     (base32 "0czcg9098c81xxgg6ajzk31dln9nwy3rb841a5w86cfkn59bx5rr"))))

(define-public linux-xanmod
  (let ((base (customize-linux #:name "linux-xanmod"
                               #:source linux-xanmod-source
                               #:defconfig "config_x86-64-v1"
                               ;; Extraversion is used instead.
                               #:configs (config->string
                                          '(("CONFIG_LOCALVERSION" . "")))
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
                                                ;; FIXME: There might be other
                                                ;; support missing.
                                                (append '(("CONFIG_BLK_DEV_NVME" . #t)
                                                          ("CONFIG_CRYPTO_XTS" . m)
                                                          ("CONFIG_VIRTIO_CONSOLE" . m))
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
