;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils file)
  ;; Guile builtins
  #:use-module (ice-9 textual-ports)
  ;; Utilities
  #:use-module (guix gexp)
  ;; Guix packages
  #:use-module (gnu packages guile-xyz)
  #:export (computed-substitution-with-inputs
            file-content
            hidden-desktop-entry

            ini-file
            yaml-file))

;; XXX: ‘substitute*’ doesn't fully support Unicode:
;; https://codeberg.org/guix/guix/src/commit/a88d6a45e422cede96d57d7a953439dc27c6a50c/guix/build/utils.scm#L964

(define (computed-substitution-with-inputs name file inputs)
  (with-imported-modules '((guix build utils))
    (computed-file
     name
     #~(begin
         (use-modules (ice-9 match)
                      (guix build utils))
         (copy-file #$file #$output)
         (substitute* #$output
           (("\\$\\$([^\\$]+)\\$\\$" _ path)
            (let loop ((candidates '#$inputs))
              (if (null? candidates)
                  (error "file '~a' not found" path)
                  (match candidates
                    ((candidate . rest)
                     (let ((full-path (in-vicinity candidate path)))
                       (if (file-exists? full-path)
                           full-path
                           (loop rest)))))))))))))

(define (file-content file)
  (call-with-input-file (canonicalize-path file) get-string-all))

(define (hidden-desktop-entry name file)
  (computed-file name
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (copy-file #$file #$output)
          (substitute* #$output
            (("^\\[Desktop Entry\\].*" all)
             (string-append all "NoDisplay=true\n")))))))


;;;
;;; Serializers.
;;;

;; https://github.com/artyom-poptsov/guile-ini
(define (ini-file name exp)
  "Return file-like object NAME, serialized from G-expression EXP in INI
format."
  (computed-file name
    (with-extensions (list guile-ini guile-lib guile-smc)
      #~(begin
          (use-modules (srfi srfi-26) (ini))
          (call-with-output-file #$output
            (cut scm->ini #$exp #:port <>))))))

;; https://gitlab.com/yorgath/guile-yamlpp
(define (yaml-file name exp)
  "Return file-like object NAME, serialized from G-expression EXP in YAML
format."
  (computed-file name
    (with-extensions (list guile-yamlpp)
      #~(begin
          (use-modules (yamlpp))
          (call-with-output-file #$output
            (lambda (port)
              (let ((emitter (make-yaml-emitter)))
                (yaml-emit! emitter #$exp)
                (display (yaml-emitter-string emitter) port))))))))
