;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal home services writable-files-manager)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:export (home-writable-files-manager-service-type))

;;; Based on (gnu home services symlink-manager).
;;; This service is a replacement to home-symlink-manager-service-type.
;;;
;;; Usage:
;;;--8<---------------cut here---------------start------------->8---
;;; (home-environment
;;;   (essential-services
;;;    (cons* (service home-writable-files-manager-service-type)
;;;           (modify-services ((@@ (gnu home) home-environment-default-essential-services)
;;;                             this-home-environment)
;;;             (delete home-symlink-manager-service-type)))
;;;    ...))
;;;--8<---------------cut here---------------end--------------->8---
;;;
;;; Files managed by Guix Home are copied as regular file and made writable.
;;; Directories are still symlinked.

(define (update-writable-files-script)
  (program-file
   "update-writable-files"
   (with-extensions (list guile-gcrypt)
     (with-imported-modules (source-module-closure
                             '((guix build utils)
                               (guix hash)
                               (guix i18n)))
       #~(begin
           (use-modules (ice-9 ftw)
                        (ice-9 match)
                        (srfi srfi-1)
                        (guix hash)
                        (guix i18n)
                        (guix build utils))

           (define home-directory
             (getenv "HOME"))

           (define xdg-config-home
             (or (getenv "XDG_CONFIG_HOME")
                 (string-append (getenv "HOME") "/.config")))

           (define xdg-data-home
             (or (getenv "XDG_DATA_HOME")
                 (string-append (getenv "HOME") "/.local/share")))

           (define backup-directory
             (string-append home-directory "/" (number->string (current-time))
                            "-guix-home-legacy-configs-backup"))

           (define (preprocess-file file)
             "If file is in XDG-CONFIGURATION-FILES-DIRECTORY use
subdirectory from XDG_CONFIG_HOME to generate a target path."
             (cond
              ((string-prefix? #$xdg-configuration-files-directory file)
               (string-append
                (substring xdg-config-home
                           (1+ (string-length home-directory)))
                (substring file
                           (string-length #$xdg-configuration-files-directory))))
              ((string-prefix? #$xdg-data-files-directory file)
               (string-append
                (substring xdg-data-home
                           (1+ (string-length home-directory)))
                (substring file
                           (string-length #$xdg-data-files-directory))))
              (else file)))

           (define (target-file file)
             ;; Return the target of FILE, a config file name sans leading dot
             ;; such as "config/fontconfig/fonts.conf" or "bashrc".
             (string-append home-directory "/" (preprocess-file file)))

           (define (no-follow-file-exists? file)
             "Return #t if file exists, even if it's a dangling symlink."
             (->bool (false-if-exception (lstat file))))

           (define (symlink-to-store? file)
             (catch 'system-error
               (lambda ()
                 (store-file-name? (readlink file)))
               (lambda args
                 (if (= EINVAL (system-error-errno args))
                     #f
                     (apply throw args)))))

           (define (backup-file file)
             (define backup
               (string-append backup-directory "/" (preprocess-file file)))

             (define (copy-file* oldfile newfile)
               "Like 'copy-file', but also copies dangling symlinks."
               (catch 'system-error
                 (lambda ()
                   (copy-file oldfile newfile))
                 (lambda args
                   (if (and (eq? 'symlink (stat:type (lstat oldfile)))
                            (= ENOENT (system-error-errno args)))
                       (symlink (readlink oldfile) newfile)
                       (apply throw args)))))

             (mkdir-p backup-directory)
             (format #t (G_ "Backing up ~a...") (target-file file))
             (mkdir-p (dirname backup))
             (copy-file* (target-file file) backup)
             (delete-file (target-file file))
             (display (G_ " done\n")))

           (define (cleanup-writable-files home-generation)
             ;; Delete from $HOME files that originate in HOME-GENERATION, the
             ;; store item containing a home generation.
             (define config-file-directory
               ;; Note: Trailing slash is needed because "files" is a symlink.
               (string-append home-generation "/" #$home-files-directory "/"))

             (define (strip file)
               (string-drop file
                            (+ 1 (string-length config-file-directory))))

             (define (source-file file)
               (readlink (string-append config-file-directory file)))

             (format #t (G_ "Cleaning up writable files from previous home at ~a.~%")
                     home-generation)
             (newline)

             (file-system-fold
              (const #t)
              (lambda (file stat _)                 ;leaf
                (let ((source (source-file (strip file)))
                      (target (target-file (strip file))))
                  (when (no-follow-file-exists? target)
                    ;; DO NOT remove the file if it is different from the one
                    ;; we'll create later, it will be backed up later during
                    ;; create-writable-files phase.
                    (if (or (symlink-to-store? target)
                            (equal? (file-hash* source)
                                    (file-hash* target)))
                        (begin
                          (format #t (G_ "Removing ~a...") target)
                          (delete-file target)
                          (display (G_ " done\n")))
                        (format
                         #t
                         (G_ "Skipping ~a (file changed)... done\n")
                         target)))))

              (const #t)                            ;down
              (lambda (directory stat _)            ;up
                (unless (string=? directory config-file-directory)
                  (let ((directory (target-file (strip directory))))
                    (catch 'system-error
                      (lambda ()
                        (rmdir directory)
                        (format #t (G_ "Removed ~a.\n") directory))
                      (lambda args
                        (let ((errno (system-error-errno args)))
                          (cond
                           ((= ENOTEMPTY errno)
                            (format
                             #t
                             (G_ "Skipping ~a (not an empty directory)... done\n")
                             directory))
                           ;; This happens when the directory is a mounted device.
                           ((= EBUSY errno)
                            (format
                             #t
                             (G_ "Skipping ~a (underlying device is busy)... done\n")
                             directory))
                           ((= ENOENT errno) #t)
                           ((= ENOTDIR errno) #t)
                           (else
                            (apply throw args)))))))))
              (const #t)                            ;skip
              (const #t)                            ;error
              #t                                    ;init
              config-file-directory
              lstat)

             (display (G_ "Cleanup finished.\n\n")))

           (define (create-writable-files home-generation)
             ;; Create in $HOME writable files for the files in HOME-GENERATION.
             (define config-file-directory
               ;; Note: Trailing slash is needed because "files" is a symlink.
               (string-append home-generation "/" #$home-files-directory "/"))

             (define (strip file)
               (string-drop file
                            (+ 1 (string-length config-file-directory))))

             (define (source-file file)
               (readlink (string-append config-file-directory file)))

             (file-system-fold
              (const #t)                            ;enter?
              (lambda (file stat result)            ;leaf
                (let ((source (source-file (strip file)))
                      (target (target-file (strip file))))
                  (when (no-follow-file-exists? target)
                    (backup-file (strip file)))
                  (format #t (G_ "Copying ~a -> ~a...")
                          source target)
                  (if (file-is-directory? source)
                      (symlink source target)
                      (begin
                        (copy-file source target)
                        (make-file-writable target)))
                  (display (G_ " done\n"))))
              (lambda (directory stat result)       ;down
                (unless (string=? directory config-file-directory)
                  (let ((target (target-file (strip directory))))
                    (when (and (no-follow-file-exists? target)
                               (not (file-is-directory? target)))
                      (backup-file (strip directory)))

                    (catch 'system-error
                      (lambda ()
                        (mkdir target))
                      (lambda args
                        (let ((errno (system-error-errno args)))
                          (unless (= EEXIST errno)
                            (format #t (G_ "failed to create directory ~a: ~s~%")
                                    target (strerror errno))
                            (apply throw args))))))))
              (const #t)                            ;up
              (const #t)                            ;skip
              (const #t)                            ;error
              #t                                    ;init
              config-file-directory))

           #$%initialize-gettext

           (let* ((home     (string-append home-directory "/.guix-home"))
                  (pivot    (string-append home ".new"))
                  (new-home (getenv "GUIX_NEW_HOME"))
                  (old-home (getenv "GUIX_OLD_HOME")))
             (when old-home
               (cleanup-writable-files old-home))

             (create-writable-files new-home)

             (symlink new-home pivot)
             (rename-file pivot home)

             (display (G_" done\nFinished updating writable files.\n\n"))))))))

(define (update-writable-files-gexp _)
  #~(primitive-load #$(update-writable-files-script)))

(define home-writable-files-manager-service-type
  (service-type (name 'home-writable-files-manager)
                (extensions
                 (list
                  (service-extension
                   home-activation-service-type
                   update-writable-files-gexp)))
                (default-value #f)
                (description "Provide an @code{update-writable-files} script,
which copies files to configuration files, makes them writable and creates
symlinks to directories on every activation.  If an existing file would be
overwritten, backs up that file first.")))
