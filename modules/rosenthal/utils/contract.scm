;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils contract)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix colors)
  #:use-module (guix diagnostics)
  #:use-module (rosenthal utils contract impl)
  #:use-module (rosenthal utils contract blame)
  #:use-module (rosenthal utils contract combinators)
  #:export (apply-contract/guix-record-field
            contract
            define/contract))

#|

Helpers to attach contracts to values and functions.

See (rosenthal utils contract impl) for more information.

|#

(define (src->location-str src)
  (match src
    ((_ _ _)
     (let ((location (source-properties->location src)))
       (if (supports-hyperlinks? (guix-warning-port))
           (location->hyperlink location)
           (location->string location))))
    (_ #f)))

(define* (apply-contract contract value positive negative name location
                         #:key context-limit)
  (let* ((ctc      (->contract contract))
         (ctc-name (contract-name ctc))
         (checker  (contract-checker ctc))
         (blame
          (make-blame*
           #:source location
           #:value name
           #:build-name (lambda () ctc-name)
           #:positive positive
           #:negative #f
           #:original? #t
           #:context-limit context-limit)))
    (cond
     (checker
      (if (checker value)
          value
          (raise-blame-error
           blame #:missing-party negative
           `(expected: "~a" got: "~s") ctc-name value)))
     (else
      (((contract-enforcer ctc) blame) value negative)))))

(define-syntax apply-contract/guix-record-field
  (lambda (stx)
    (syntax-case stx ()
      ((_ ctc value record-name field-name)
       (with-syntax ((field-name #'(format #f "(field: ~a)" field-name))
                     (location (src->location-str (syntax-source stx))))
         ;; Use field-name for both positive and negative parties, to ensure the
         ;; error message is always "contract violation".
         #'(apply-contract ctc value field-name field-name record-name location))))))

(define-syntax contract
  (lambda (stx)
    (syntax-case stx ()
      ((_ ctc value positive negative name location #:context-limit context-limit)
       #'(apply-contract ctc value positive negative name location #:context-limit context-limit))
      ((_ ctc value positive negative name location)
       #'(contract ctc value positive negative name location #:context-limit #f))
      ((_ ctc value positive negative name)
       (with-syntax ((location (src->location-str (syntax-source stx))))
         #'(contract ctc value positive negative name location)))
      ((_ ctc value positive negative)
       #'(contract ctc value positive negative positive))
      ((_ ctc value)
       #'(contract ctc value 'unknown-definition 'caller)))))

(define-syntax define/contract
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name . args) ctc body ...)
       (if (null? #'(body ...))
           (syntax-violation 'define/contract "missing body" stx)
           (with-syntax ((value #'(lambda* args body ...)))
             #'(define name
                 (contract ctc value '(definition: name) 'caller 'name)))))
      ((_ name ctc body ...)
       (if (null? #'(body ...))
           (syntax-violation 'define/contract "missing body" stx)
           (with-syntax ((value #'(begin body ...)))
             #'(define name
                 (contract ctc value '(definition: name) 'caller 'name))))))))
