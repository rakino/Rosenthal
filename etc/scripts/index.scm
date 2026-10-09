#!/usr/bin/env -S guix repl --
!#
;;; SPDX-License-Identifier: CC0-1.0
;;; Copyright © 2025-2026 Hilton Chain <hako@ultrarare.space>

(use-modules (ice-9 match)
             (ice-9 string-fun)
             (srfi srfi-1)
             (srfi srfi-71)
             (guix diagnostics)
             (guix discovery)
             (guix packages)
             (guix profiles)
             (guix ui)
             (rosenthal utils packages)
             (rosenthal utils services)
             (gnu services))

(define (location->org-url loc)
  (match loc
    (#f "<unknown location>")
    (($ <location> file line column)
     (format #f "[[~a][~a]]"
             (format #f "https://codeberg.org/hako/Rosenthal/src/branch/trunk/modules/~a#L~a"
                     file
                     line)
             (basename file)))))

(define (replace-newlines str)
  (define str*
    (string-replace-substring
     (string-trim-right str #\newline)
     "\n"
     " "))
  (if (string-null? str*)
      " "
      str*))

(format #t "\
*** Packages
| PACKAGE | SYNOPSIS | LOCATION |
|-+-+-|
")

(define (sort-packages packages)
  (stable-sort
   packages
   (lambda (a b)
     (string< (package-name a)
              (package-name b)))))

(for-each
 (lambda (p)
   (format #t "|[[~a][=~a=]]@~a|~a|~a|~%"
           (package-home-page p)
           (package-name p)
           (package-version p)
           (replace-newlines (package-synopsis-string p))
           (and=> (package-location p) location->org-url)))
 (sort-packages
  (filter (negate hidden-package?)
          (all-rosenthal-packages))))


(format #t "\
*** Services
| SERVICE | DESCRIPTION | LOCATION |
|-+-+-|
")

;; Copied from (guix scripts system search).
(define service-type-name*
  (compose symbol->string service-type-name))
(define (service-type-description-string type)
  "Return the rendered and localised description of TYPE, a service type."
  (and=> (service-type-description type)
         texi->plain-text))

(define (sort-services services)
  (stable-sort
   services
   (lambda (a b)
     (string< (service-type-name* a)
              (service-type-name* b)))))

(for-each
 (lambda (s)
   (format #t "|=~a=|~a|~a|~%"
           (service-type-name* s)
           (replace-newlines (service-type-description-string s))
           (and=> (service-type-location s) location->org-url)))
 (let ((home-services
        services
        (partition (lambda (s)
                     (string-prefix? "home-" (service-type-name* s)))
                   (all-rosenthal-services))))
   (append
    (sort-services services)
    (sort-services home-services))))
