;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
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


;;;
;;; Linux-XanMod
;;;

(define (make-linux-xanmod-source version xanmod-revision hash-string)
  (origin
    (method url-fetch)
    (uri (string-append "https://gitlab.com/xanmod/linux/-/archive/"
                        version "-" xanmod-revision ".tar.bz2"))
    (sha256 hash-string)))

(define* (make-linux-xanmod version xanmod-revision source
                            #:key
                            (name "linux-xanmod")
                            (xanmod-defconfig "config_x86-64-v1"))
  (let ((defconfig xanmod-defconfig)    ;to be used in phases.
        (base (customize-linux #:name name
                               #:source source
                               #:defconfig xanmod-defconfig
                               ;; EXTRAVERSION is used instead.
                               #:configs (config->string
                                          '(("CONFIG_LOCALVERSION" . "")))
                               #:extra-version xanmod-revision)))
    (package
      (inherit base)
      (version version)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              ;; EXTRAVERSION is used instead.
              (add-after 'unpack 'remove-localversion
                (lambda _
                  (when (file-exists? "localversion")
                    (delete-file "localversion"))))
              (add-before 'configure 'add-xanmod-defconfig
                (lambda _
                  (rename-file
                   (string-append "CONFIGS/xanmod/gcc/" #$defconfig)
                   ".config")

                  ;; Adapted from `make-linux-libre*'.
                  (chmod ".config" #o666)
                  (let ((port (open-file ".config" "a"))
                        (extra-configuration
                         #$(config->string
                            ;; FIXME: There might be other support missing.
                            (append '(("CONFIG_BLK_DEV_NVME" . #t)
                                      ("CONFIG_CRYPTO_XTS" . m)
                                      ("CONFIG_VIRTIO_CONSOLE" . m))
                                    %default-extra-linux-options))))
                    (display extra-configuration port)
                    (close-port port))
                  (invoke "make" "oldconfig")

                  (rename-file
                   ".config"
                   (string-append "arch/x86/configs/" #$defconfig))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         ;; cpio is needed for CONFIG_IKHEADERS.
         (append cpio zstd)))
      (home-page "https://xanmod.org/")
      (supported-systems '("x86_64-linux"))
      (synopsis
       "Linux kernel distribution with custom settings and new features")
      (description
       "This package provides XanMod kernel, a general-purpose Linux kernel
distribution with custom settings and new features.  It's built to provide a
stable, responsive and smooth desktop experience."))))

(define linux-xanmod-version "6.3.8")
(define linux-xanmod-revision "xanmod1")
(define linux-xanmod-source
  (make-linux-xanmod-source
   linux-xanmod-version
   linux-xanmod-revision
   (base32 "07lfcbmjyjvndk0hz06s8s242n4dhc03drh8bcxi4b1hawn54g9p")))

(define-public linux-xanmod
  (make-linux-xanmod linux-xanmod-version
                     linux-xanmod-revision
                     linux-xanmod-source))
