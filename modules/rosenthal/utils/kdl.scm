;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils kdl)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (scm->kdl-string
            scm->kdlv1-string))

;; https://kdl.dev/spec/

(define %kdl-v1?
  (make-parameter #f))

(define* (value->kdl value #:optional (kdlv1? (%kdl-v1?)))
  (match value
    ;; Null
    ('null
     (if kdlv1? "null" "#null"))
    ;; Boolean
    ((or #t 'true)
     (if kdlv1? "true" "#true"))
    ((or #f 'false)
     (if kdlv1? "false" "#false"))
    ;; Number
    ((or +inf.0 'inf)
     "#inf")
    ((or -inf.0 '-inf)
     "#-inf")
    ((or +nan.0 'nan)
     "#nan")
    ((? number?)
     (number->string value))
    ;; String
    ((? symbol?)
     (symbol->string value))
    ((? string?)
     (format #f "~s" value))
    ;; Property.  Added here for convenience.
    (_
     (string-join
      (map (lambda (x)
             (format #f "~a=~a"
                     (value->kdl (first x))
                     (value->kdl (second x))))
           (cdr value))))))

(define (child-node? x)
  (and (pair? x)
       (not (eq? (car x) '@))))

(define (node->kdl node level)
  (let* ((name (car node))
         (children args (partition child-node? (cdr node)))
         (indent (make-string (* level 8) #\space))
         (output
          (string-join
           (cons (string-append indent (value->kdl name))
                 (map value->kdl args)))))
    (if (null? children)
        output
        (format #f "~a {~%~{~a~%~}~a}"
                output
                (map (cut node->kdl <> (1+ level)) children)
                indent))))

(define (document->kdl document)
  (map (cut node->kdl <> 0) document))

(define (scm->kdl-string scm)
  (cond
   ((null? scm)
    "")
   ((not (pair? scm))
    (value->kdl scm))
   ((eq? (car scm) '*TOP*)
    (scm->kdl-string (cdr scm)))
   (else
    (string-join (document->kdl scm) "\n"))))

(define (scm->kdlv1-string scm)
  (parameterize ((%kdl-v1? #t))
    (scm->kdl-string scm)))
