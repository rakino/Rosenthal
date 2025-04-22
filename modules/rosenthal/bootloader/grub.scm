;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (rosenthal packages bootloaders)
  #:export (grub-efi-luks2-bootloader
            grub-efi-luks2-removable-bootloader))

(define grub-efi-luks2-bootloader
  (bootloader
   (inherit grub-efi-bootloader)
   ;; NOTE: Don't change the name.  Generation switching code only knows
   ;; bootloaders defined in (gnu bootloader grub).
   (name 'grub-efi)
   (package grub-efi-luks2)))

(define grub-efi-luks2-removable-bootloader
  (bootloader
   (inherit grub-efi-removable-bootloader)
   (package grub-efi-luks2)))
