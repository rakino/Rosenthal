;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal bootloader limine)
  ;; Guile builtins
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  ;; Guix System - bootloaders
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (rosenthal bootloader uki)
  ;; Guix packages
  #:use-module (rosenthal packages bootloaders)
  #:export (limine-efi-removable-bootloader))

(define script-path "/boot/install-limine.scm")

(define* (limine-configuration-file config entries
                                    #:key (old-entries '()) #:allow-other-keys)
  (define limine
    (file-append
     (bootloader-package (bootloader-configuration-bootloader config))
     (format #f "/share/limine/BOOT~a.EFI"
             (cond
              ((target-x86-32?) "IA32")
              ((target-x86-64?) "X64")
              ((target-aarch64?) "AA64")
              ((target-riscv64?) "RISCV64")))))

  (define menu-entries
    (cons (first entries)
          old-entries))

  (define labels
    (map menu-entry-label menu-entries))

  (define ukify-args
    (map menu-entry->ukify-args menu-entries))

  (program-file "install-limine"
    (with-imported-modules
        (source-module-closure
         '((guix build syscalls)
           (guix build utils)))
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-1)
                       (guix build syscalls)
                       (guix build utils))
          (let* ((args (command-line))
                 (directory (second args))
                 (limine-directory (in-vicinity directory "EFI/BOOT"))
                 (guix-directory (in-vicinity directory "EFI/Guix")))
            (for-each mkdir-p (list limine-directory guix-directory))
            ;; Install unified kernel images and generate Limine configuration.
            (call-with-output-file (in-vicinity limine-directory "limine.conf.tmp")
              (lambda (port)
                (let ((ukify #$(file-append ukify "/bin/ukify"))
                      (current-label (first '#$labels))
                      (current-args  (first (list #$@ukify-args)))
                      (old-labels    (cdr   '#$labels))
                      (old-args      (cdr   (list #$@ukify-args))))
                  (format port "\
timeout: 5~%")
                  (with-directory-excursion guix-directory
                    (for-each delete-file (find-files "." "^OLD-[0-9]+\\.EFI$"))
                    (apply invoke/quiet ukify "build" "--output" "CURRENT.EFI"
                           current-args)
                    (format port "
/~a
  protocol: efi
  path: boot():/EFI/Guix/CURRENT.EFI~%"
                            current-label)
                    (unless (null? old-labels)
                      (format port "
/GNU system, old configurations...~%"))
                    (let loop ((count 1)
                               (labels old-labels)
                               (args old-args))
                      (let ((image-name (format #f "OLD-~a.EFI" count)))
                        (unless (null? labels)
                          (catch #t
                            (lambda ()
                              (apply invoke/quiet
                                     ukify "build" "--output" image-name
                                     (first args))
                              (format port "~
//~a
  protocol: efi
  path: boot():/EFI/Guix/OLD-~a.EFI~%"
                                      (first labels)
                                      count))
                            (lambda _
                              (delete-file image-name)
                              ;; Exit loop.
                              (loop 0 '() '())))
                          (loop (1+ count)
                                (cdr labels)
                                (cdr args)))))))
                (rename-file (in-vicinity limine-directory "limine.conf.tmp")
                             (in-vicinity limine-directory "limine.conf"))
                ;; Finally, install Limine.
                (install-file #$limine limine-directory))))))))

(define install-limine-efi
  #~(lambda (bootloader target mount-point)
      (invoke (string-append mount-point #$script-path)
              (string-append mount-point target))))

;; Limine + UKI
;; NOTE: ‘configuration-file’ here is actually an activation script to be invoked by
;; ‘installer’.
;; XXX: Not expected by ‘reinstall-bootloader’ so rolling-back and switching
;; generation won't work.
(define limine-efi-removable-bootloader
  (bootloader
    ;; NOTE: Don't change the name.  Generation switching code only knows
    ;; bootloaders defined in (gnu bootloader grub).
    (name 'grub-efi-removable-bootloader)
    (package limine)
    (installer install-limine-efi)
    (disk-image-installer #f)
    (configuration-file script-path)
    (configuration-file-generator limine-configuration-file)))
