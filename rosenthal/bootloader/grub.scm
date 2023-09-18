;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (rosenthal packages bootloaders)
  #:export (grub-efi-luks2-bootloader))

(define grub-efi-luks2-bootloader
  (bootloader
   (inherit grub-efi-bootloader)
   (name 'grub-efi-luks2-bootloader)
   (package grub-efi-luks2)))
