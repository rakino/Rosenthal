#!/usr/bin/env -S guix repl --
!#
;;; SPDX-License-Identifier: CC0-1.0
;;; Copyright © 2025-2026 Hilton Chain <hako@ultrarare.space>

(use-modules (guix packages)
             (guix profiles)
             (guix ui)
             (rosenthal utils packages))

(format #t "*** Packages~%")
(for-each
 (lambda (p)
   (format #t "- [[~a][~a]]@~a :: ~a~%"
           (package-home-page p)
           (package-name p)
           (package-version p)
           (string-drop-right
            (package-synopsis-string p)
            (string-length "\n\n"))))
 (stable-sort
  (filter (negate hidden-package?)
          (all-rosenthal-packages))
  (lambda (a b)
    (string< (package-name a)
             (package-name b)))))
