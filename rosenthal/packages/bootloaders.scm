;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages bootloaders)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages python))

;; Patches obtained from:
;; <https://leo3418.github.io/collections/gentoo-config-luks2-grub-systemd/packages.html>

(define grub-runtime-memregion-alloc-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://leo3418.github.io/"
                        "res/collections/gentoo-config-luks2-grub-systemd/"
                        "4500-grub-2.06-runtime-memregion-alloc.patch"))
    (sha256
     (base32
      "1ynlyaabdlw7g4pqr4sc7jdqn9riwms637cqhlqnyi8jp6cs4a6x"))))

(define grub-luks2-argon2-support-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://leo3418.github.io/"
                        "res/collections/gentoo-config-luks2-grub-systemd/"
                        "5000-grub-2.06-luks2-argon2-v4.patch"))
    (sha256
     (base32
      "0jfyl2j9bh0225iqq36gy55p2l96zhhhsk5r53aqmlivvxpjlf7g"))))

(define grub-install-luks2-fix-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://leo3418.github.io/"
                        "res/collections/gentoo-config-luks2-grub-systemd/"
                        "9500-grub-AUR-improved-luks2.patch"))
    (sha256
     (base32
      "0hn0nkp5cfsyvvyfj3vbw0z4c2swdi6gvxradl401lqvwsvq60p4"))))

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
                    (list grub-runtime-memregion-alloc-patch
                          grub-luks2-argon2-support-patch
                          grub-install-luks2-fix-patch))))))
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
         (append autoconf automake python-minimal-wrapper))))))
