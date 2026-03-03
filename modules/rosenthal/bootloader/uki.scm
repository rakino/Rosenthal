;;; Copyright © 2024 Lilah Tascheter <lilah@lunabee.space>
;;; Copyright © 2024, 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal bootloader uki)
  ;; Guile builtins
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  ;; Guix System - bootloaders
  #:use-module (gnu bootloader)
  ;; Guix packages
  #:use-module (rosenthal packages bootloaders)
  #:export (menu-entry->ukify-args
            uefi-uki-removable-bootloader))

(define script-path "/boot/install-uki.scm")

(define (menu-entry->ukify-args entry)
  (let* ((label     (menu-entry-label entry))
         (linux     (menu-entry-linux entry))
         (initrd    (menu-entry-initrd entry))
         (arguments (menu-entry-linux-arguments entry)))
    #~(list "--os-release" #$label
            "--linux" #$linux
            "--initrd" #$initrd
            "--cmdline" (string-join (list #$@arguments))
            "--stub"
            #$(file-append systemd-stub "/libexec/" (systemd-stub-name)))))

(define (uefi-uki-configuration-file config entries . rest)
  (program-file "install-uki"
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (ice-9 match)
                       (guix build utils))
          (let* ((directory (second (command-line)))
                 (installation-path
                  (in-vicinity
                   directory
                   #$(format #f "/EFI/BOOT/BOOT~a.EFI"
                             (cond
                              ((target-x86-32?) "IA32")
                              ((target-x86-64?) "X64")
                              ((target-arm32?) "ARM")
                              ((target-aarch64?) "AA64")
                              ((target-riscv64?) "RISCV64"))))))
            (mkdir-p (dirname installation-path))
            (apply invoke #$(file-append ukify "/bin/ukify")
                   "build" "--output" installation-path
                   #$(menu-entry->ukify-args (first entries))))))))

(define install-uefi-uki
  #~(lambda (bootloader target mount-point)
      (invoke (string-append mount-point #$script-path)
              (string-append mount-point target))))

;; ‘configuration-file’ here is actually an activation script to be invoked by
;; ‘installer’.
;; FIXME: Not expected by ‘reinstall-bootloader’ in (guix scripts system).
;; NOTE: Faking name here to support rolling-back with '--no-bootloader' option.
(define uefi-uki-removable-bootloader
  (bootloader
   (name 'grub-efi-removable-bootloader)
   (package systemd-stub)
   (installer install-uefi-uki)
   (disk-image-installer #f)
   (configuration-file script-path)
   (configuration-file-generator uefi-uki-configuration-file)))
