;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils file)
  ;; Guile builtins
  #:use-module (ice-9 textual-ports)
  ;; Utilities
  #:use-module (guix gexp)
  ;; Guix packages
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (rosenthal packages guile-xyz)
  #:export (computed-substitution-with-inputs
            file-content
            hidden-desktop-entry

            ini-file
            json-file
            kdl-file
            kdlv1-file
            toml-file
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
                           (loop rest))))))))))
     #:options '(#:substitutable? #f))))

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
             (string-append all "NoDisplay=true\n")))))
    #:options '(#:substitutable? #f)))


;;;
;;; Serializers.
;;;

;; https://github.com/artyom-poptsov/guile-ini
(define (ini-file name exp)
  "Return a file-like object, outputing INI file serialized from EXP."
  (computed-file name
    (with-extensions (list guile-ini guile-lib guile-smc)
      (if (gexp? exp)
          #~(begin
              (use-modules (srfi srfi-26) (ini))
              (call-with-output-file #$output
                (cut scm->ini #$exp #:port <>)))
          #~(begin
              (use-modules (srfi srfi-26) (ini))
              (call-with-output-file #$output
                (cut scm->ini '#$exp #:port <>)))))
    #:options '(#:substitutable? #f)))

;; https://github.com/aconchillo/guile-json
(define (json-file name exp)
  "Return a file-like object, outputing JSON file serialized from EXP."
  (computed-file name
    (with-extensions (list guile-json-4)
      (if (gexp? exp)
          #~(begin
              (use-modules (srfi srfi-26) (json))
              (call-with-output-file #$output
                (cut scm->json #$exp <> #:pretty #t)))
          #~(begin
              (use-modules (srfi srfi-26) (json))
              (call-with-output-file #$output
                (cut scm->json '#$exp <> #:pretty #t)))))
    #:options '(#:substitutable? #f)))

(define (kdl-file name exp)
  (computed-file name
    (with-imported-modules '((rosenthal utils kdl))
      (if (gexp? exp)
          #~(begin
              (use-modules (srfi srfi-26) (rosenthal utils kdl))
              (call-with-output-file #$output
                (cut display (scm->kdl-string #$exp) <>)))
          #~(begin
              (use-modules (srfi srfi-26) (rosenthal utils kdl))
              (call-with-output-file #$output
                (cut display (scm->kdl-string '#$exp) <>)))))
    #:options '(#:substitutable? #f)))

(define (kdlv1-file name exp)
  (computed-file name
    (with-imported-modules '((rosenthal utils kdl))
      (if (gexp? exp)
          #~(begin
              (use-modules (srfi srfi-26) (rosenthal utils kdl))
              (call-with-output-file #$output
                (cut display (scm->kdlv1-string #$exp) <>)))
          #~(begin
              (use-modules (srfi srfi-26) (rosenthal utils kdl))
              (call-with-output-file #$output
                (cut display (scm->kdlv1-string '#$exp) <>)))))
    #:options '(#:substitutable? #f)))

;; https://github.com/hylophile/guile-toml
;; TODO: TOML writing support is incomplete.
;; See https://github.com/hylophile/guile-toml/blob/main/toml/builder.scm.
(define (toml-file name exp)
  "Return a file-like object, outputing TOML file serialized from EXP."
  (computed-file name
    (with-extensions (list guile-json-4 guile-toml/dolly)
      (if (gexp? exp)
          #~(begin
              (use-modules (srfi srfi-26) (toml))
              (call-with-output-file #$output
                (cut scm->toml #$exp <>)))
          #~(begin
              (use-modules (srfi srfi-26) (toml))
              (call-with-output-file #$output
                (cut scm->toml '#$exp <>)))))
    #:options '(#:substitutable? #f)))

;; https://gitlab.com/yorgath/guile-yamlpp
(define (yaml-file name exp)
  "Return a file-like object, outputing YAML file serialized from EXP."
  (computed-file name
    (with-extensions (list guile-yamlpp)
      (if (gexp? exp)
          #~(begin
              (use-modules (yamlpp))
              (call-with-output-file #$output
                (lambda (port)
                  (let ((emitter (make-yaml-emitter)))
                    (yaml-emit! emitter #$exp)
                    (display (yaml-emitter-string emitter) port)))))
          #~(begin
              (use-modules (yamlpp))
              (call-with-output-file #$output
                (lambda (port)
                  (let ((emitter (make-yaml-emitter)))
                    (yaml-emit! emitter '#$exp)
                    (display (yaml-emitter-string emitter) port)))))))
    #:options '(#:substitutable? #f)))
