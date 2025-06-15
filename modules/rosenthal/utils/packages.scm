;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils packages)
  #:use-module (gnu packages)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix i18n)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-34)
  #:replace (%patch-path
             search-patch)
  #:export (rosenthal-patches
            %rosenthal-package-module-path
            all-rosenthal-packages

            rosenthal-disable-updater?
            delete-package-from-list

            spec->pkg
            spec->pkg+out
            specs->pkgs
            specs->pkgs+out))

(define %rosenthal-root-directory
  ;; This is like %distro-root-directory from (gnu packages), with adjusted
  ;; paths.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("rosenthal/packages/binaries.scm" rosenthal/ packages/)
         ("rosenthal/packages.scm" rosenthal/))))

(define %rosenthal-package-module-path
  `((,%rosenthal-root-directory . "rosenthal/packages")))

(define %patch-path
  ;; Define it after '%package-module-path' so that '%load-path' contains user
  ;; directories, allowing patches in $GUIX_PACKAGE_PATH to be found.
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %rosenthal-root-directory)
              (string-append directory "/rosenthal/packages/patches")
              directory))
        %load-path)))

;;; XXX: The following must be redefined to make use of the overridden
;;; %patch-path parameter above.
(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

;;; XXX: `search-patches' being syntax, it can't be overridden by the module
;;; system, or so it seems, so we simply rename it.
(define-syntax-rule (rosenthal-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

;; Adapted from (@ (gnu packages) all-packages).
(define all-rosenthal-packages
  (mlambda ()
    "Return the list of all public packages, including replacements and hidden
packages, excluding superseded packages."
    ;; Note: 'fold-packages' never traverses the same package twice but
    ;; replacements break that (they may or may not be visible to
    ;; 'fold-packages'), hence this hash table to track visited packages.
    (define visited (make-hash-table))

    (fold-packages (lambda (package result)
                     (if (hashq-ref visited package)
                         result
                         (begin
                           (hashq-set! visited package #t)
                           (match (package-replacement package)
                             ((? package? replacement)
                              (hashq-set! visited replacement #t)
                              (cons* replacement package result))
                             (#f
                              (cons package result))))))
                   '()
                   (all-modules %rosenthal-package-module-path #:warn warn-about-load-error)
                   ;; Dismiss deprecated packages but keep hidden packages.
                   #:select? (negate package-superseded))))



(define (rosenthal-disable-updater? p)
  (assq-ref (package-properties p) 'disable-updater?))

(define (delete-package-from-list name lst)
  "Return a copy of package list LST, removing packages named NAME."
  (filter (lambda (pkg)
            (not (string=? name (package-name pkg))))
          lst))

(define (spec->pkg spec)
  (specification->package spec))

(define (spec->pkg+out spec)
  (specification->package+output spec))

(define (specs->pkgs . specs)
  (map spec->pkg specs))

(define (specs->pkgs+out . specs)
  (map spec->pkg+out specs))

(define-deprecated/public-alias pkg spec->pkg)
(define-deprecated/public-alias pkg+out spec->pkg+out)
(define-deprecated/public-alias pkgs specs->pkgs)
(define-deprecated/public-alias pkgs+out specs->pkgs+out)
