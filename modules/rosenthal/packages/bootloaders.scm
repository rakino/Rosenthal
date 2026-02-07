;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Lilah Tascheter <lilah@lunabee.space>

(define-module (rosenthal packages bootloaders)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  ;; Guix packages
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:export (systemd-stub-name))

;; Patches obtained from:
;; <https://leo3418.github.io/collections/gentoo-config-luks2-grub-systemd/packages.html>

(define grub-luks2-argon2-support-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://leo3418.github.io/"
                        "res/collections/gentoo-config-luks2-grub-systemd/"
                        "grub-2.12-luks2-argon2-v4.patch"))
    (sha256
     (base32
      "02y15k6rd5vj2shfijyhq2nr2775vpa55ijfy6bb8irpnh8i2272"))))

(define-public grub-efi-luks2
  (let ((base grub-efi))
    (package
      (inherit base)
      (name "grub-efi-luks2")
      (source
       (let ((base (package-source base)))
         (origin
           (inherit base)
           (patches
            (append (origin-patches base)
                    (list grub-luks2-argon2-support-patch))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          #~(append #$flags '("--disable-werror")))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'delete-configure-script
                (lambda _
                  (delete-file "configure")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append autoconf automake python-minimal-wrapper)))
      (properties
       `(,@(package-properties base)
         (disable-updater? . #t))))))


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
    (version "259")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/systemd/systemd")
                     (commit (string-append "v" version))))
              (file-name (git-file-name "name" version))
              (sha256
               (base32
                "1j0f710m6h2vlry6a62q8qhvkv4c4gah6s7n212fi8liqpb1g5cl"))))
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
    (version "259")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/systemd/systemd")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j0f710m6h2vlry6a62q8qhvkv4c4gah6s7n212fi8liqpb1g5cl"))))
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
