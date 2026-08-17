;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Lilah Tascheter <lilah@lunabee.space>

(define-module (rosenthal packages bootloaders)
  #:use-module (srfi srfi-26)
  ;; Utilities
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  ;; Guix packages
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:export (systemd-stub-name))

(define-deprecated-package grub-efi-luks2 grub-efi)

(define-public limine
  (package
    (name "limine")
    (version "11.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Limine-Bootloader/Limine/releases"
                    "/download/v" version "/limine-" version ".tar.xz"))
              (sha256
               (base32
                "0d0221xc3374xq6bx26dkmidcf6slxc6dq6qnjdc3lrx3xda21jw"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list (string-append
                    "TOOLCHAIN_FOR_TARGET="
                    #$(or (and=> (%current-target-system)
                                 (cut string-append <> "-"))
                          ""))
                   (string-append "--prefix=" #$output)
                   #$@(if (target-x86?)
                          '("--enable-bios-cd"
                            "--enable-bios-pxe"
                            "--enable-bios"
                            "--enable-uefi-ia32")
                          '())
                   #$@(if (target-x86-64?)
                          '("--enable-uefi-x86-64")
                          '())
                   #$@(if (target-aarch64?)
                          '("--enable-uefi-aarch64")
                          '())
                   #$@(if (target-riscv64?)
                          '("--enable-uefi-riscv64")
                          '())
                   #$@(if (target-loongarch64?)
                          '("--enable-uefi-loongarch64")
                          '()))
           #:make-flags
           #~(list "SHELL=sh")))
    (native-inputs (list mtools nasm))
    (home-page "https://github.com/Limine-Bootloader/Limine")
    (synopsis "Multiprotocol bootloader and boot manager")
    (description
     "Limine is a multiprotocol bootloader and boot manager.  It's also used as
the reference implementation for the Limine boot protocol.")
    (license license:bsd-2)
    (properties '((disable-updater? . #t)))))


;;;
;;; Unified Kernel Image support.
;;;

(define (systemd-stub-name)
  (let ((arch (cond ((target-x86-32?) "ia32")
                    ((target-x86-64?) "x64")
                    ((target-arm32?) "arm")
                    ((target-aarch64?) "aa64")
                    ((target-riscv64?) "riscv64"))))
    (string-append "linux" arch ".efi.stub")))

(define-public systemd-stub
  (package
    (name "systemd-stub")
    (version "261.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/systemd/systemd")
                     (commit (string-append "v" version))))
              (file-name (git-file-name "name" version))
              (sha256
               (base32
                "014mjygixfh9j1p1f09xgy114v5lfah6hwh8xcscy1nqxk3p2hf3"))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags
           `(list "-Defi=true" "-Dsbat-distro=guix"
                  "-Dsbat-distro-generation=1" ; package revision!
                  "-Dsbat-distro-summary=Guix System"
                  "-Dsbat-distro-url=https://guix.gnu.org"
                  ,(string-append "-Dsbat-distro-pkgname=" name)
                  ,(string-append "-Dsbat-distro-version=" version))
           #:phases
           #~(let ((stub #$(string-append "src/boot/" (systemd-stub-name))))
               (modify-phases %standard-phases
                 (replace 'build
                   (lambda* (#:key parallel-build? #:allow-other-keys)
                     (invoke "ninja" stub
                             "-j" (if parallel-build?
                                      (number->string (parallel-job-count)) "1"))))
                 (replace 'install
                   (lambda _
                     (install-file stub (string-append #$output "/libexec"))))))))
    (inputs (list libcap libxcrypt python-pyelftools `(,util-linux "lib")))
    (native-inputs (list gperf pkg-config python-minimal python-jinja2))
    (home-page "https://systemd.io/")
    (synopsis "Unified kernel image UEFI stub")
    (description "Simple UEFi boot stub that loads a conjoined kernel image and
supporting data to their proper locations, before chainloading to the kernel.
Supports measured and/or verified boot environments.")
    (license license:lgpl2.1+)))

(define-public ukify
  (package
    (name "ukify")
    (version "261.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/systemd/systemd")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "014mjygixfh9j1p1f09xgy114v5lfah6hwh8xcscy1nqxk3p2hf3"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'check)
               (replace 'install
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((bin (string-append #$output "/bin"))
                          (file (string-append bin "/ukify"))
                          (binutils (assoc-ref inputs "binutils"))
                          (sbsign (assoc-ref inputs "sbsigntools")))
                     (mkdir-p bin)
                     (copy-file "src/ukify/ukify.py" file)
                     (wrap-program file
                       `("PATH" ":" prefix
                         (,(string-append binutils "/bin")
                          ,(string-append sbsign "/bin"))))))))))
    (inputs (list binutils python-cryptography python-pefile sbsigntools))
    (native-inputs (list python-setuptools))
    (home-page "https://systemd.io")
    (synopsis "Unified kernel image UEFI tool")
    (description "@command{ukify} joins together a UKI stub, linux kernel, initrd,
kernel arguments, and optional secure boot signatures into a single, UEFI-bootable
image.")
    (license license:lgpl2.1+)))
