;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils file)
  #:use-module (ice-9 textual-ports)
  #:use-module (guix gexp)
  #:export (computed-substitution-with-inputs
            file-content
            hidden-desktop-entry))

;; XXX: ‘substitute*’ doesn't fully support Unicode:
;; https://codeberg.org/guix/guix/src/commit/a88d6a45e422cede96d57d7a953439dc27c6a50c/guix/build/utils.scm#L964

(define (computed-substitution-with-inputs name file inputs)
  (with-imported-modules '((guix build utils))
    (computed-file
     name
     #~(begin
         (use-modules (guix build utils))
         (copy-file #$file #$output)
         (substitute* #$output
           (("\\$\\$([^\\$]+)\\$\\$" _ path)
            (search-path '#$inputs path)))))))

(define (file-content file)
  (call-with-input-file (canonicalize-path file) get-string-all))

(define (hidden-desktop-entry name file)
  (computed-file name
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (copy-file #$file #$output)
          (substitute* #$output
            (("[Desktop Entry]" all)
             (string-append all "NoDisplay=true\n")))))))
