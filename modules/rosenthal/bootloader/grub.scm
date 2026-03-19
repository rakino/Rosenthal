;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal bootloader grub)
  ;; Utilities
  #:use-module (guix deprecation)
  ;; Guix System - bootloaders
  #:use-module (gnu bootloader grub)
  #:export (grub-efi-luks2-bootloader
            grub-efi-luks2-removable-bootloader))

(define-deprecated/alias grub-efi-luks2-bootloader grub-efi-bootloader)
(define-deprecated/alias grub-efi-luks2-removable-bootloader grub-efi-removable-bootloader)
