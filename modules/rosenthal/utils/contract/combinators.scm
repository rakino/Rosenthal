;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils contract combinators)
  #:use-module (ice-9 match)
  #:autoload   (rnrs bytevectors) (bytevector?)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (guix memoization)
  #:use-module (rosenthal utils contract impl)
  #:use-module (rosenthal utils contract blame)
  #:export (any/c
            none/c
            or/c
            listof/c
            ->/c

            ->contract))

#|

This module implements contract combinators.  To avoid name collisions, all
combinator names have a ‘/c’ suffix.

|#

(define any/c
  (make-contract
   #:name "any/c"
   #:checker (const #t)))

(define none/c
  (make-contract
   #:name "none/c"
   #:checker (const #f)))

(define or/c
  (memoize
   (lambda (. ctcs)
     (let* ((contracts (map ->contract ctcs))
            (enforcers (map contract-enforcer contracts))
            (name      (compound-contract-name "or/c" contracts) ))
       (make-contract
        #:name name
        #:enforcer
        (lambda (blame)
          (define procs
            (map (lambda (enforcer)
                   (enforcer blame))
                 enforcers))
          (lambda (val neg-party)
            (let loop ((procs procs))
              (if (null? procs)
                  (raise-blame-error blame #:missing-party neg-party
                                     '(expected: "~a" got: "~s")
                                     name val)
                  (match procs
                    ((proc . rest)
                     (guard (c ((contract-violation? c)
                                (loop rest)))
                       (proc val neg-party)))))))))))))

(define listof/c
  (mlambda (ctc)
    (let* ((contract (->contract ctc))
           (enforcer (contract-enforcer contract)))
      (make-contract
       #:name (compound-contract-name "listof/c" (list contract))
       #:enforcer
       (lambda (blame)
         (define proc
           (enforcer (blame-add-context blame "an element of")))
         (lambda (val neg-party)
           (unless (list? val)
             (raise-blame-error blame #:missing-party neg-party
                                '(expected: "a list of ~a" got: "~s")
                                (contract-name contract)
                                val))
           (map (lambda (element)
                  (proc element neg-party))
                val)))))))

;; TODO: Rewrite as a macro to support define*, multiple return values, and
;; ‘any’.
(define ->/c
  (memoize
   (lambda (. ctcs)
     (let* ((contracts        (map ->contract ctcs))
            (domain-contracts (drop-right contracts 1))
            (range-contract   (last contracts))
            (contract-arity   (length domain-contracts)))
       (make-contract
        #:name (compound-contract-name "->/c" contracts)
        #:enforcer
        (lambda (blame)
          (define domain-blames
            (map (lambda (index)
                   (let ((position (format #f "the #~a argument of" index)))
                     (blame-add-context blame position #:swap? #t)))
                 (iota contract-arity 1)))
          (define domain-enforcers
            (map contract-enforcer domain-contracts))
          (define range-proc
            (let ((range-blame (blame-add-context blame "the range of")))
              ((contract-enforcer range-contract) range-blame)))
          (lambda (val neg-party)
            (unless (procedure? val)
              (raise-blame-error blame #:missing-party neg-party
                                 '(expected: "a procedure" got: "~s") val))
            (lambda args
              ;; XXX: Can't check procedure arity in Guile.
              (let ((arity (length args)))
                (unless (= arity contract-arity)
                  (raise-blame-error
                   (blame-swap blame) #:missing-party neg-party
                   `("assuming arity of the contract is always correct"
                     expected: "~a argument(s)" got: "~a argument(s)")
                   contract-arity arity)))
              (define args*
                (map (lambda (arg blame enforcer)
                       ((enforcer blame) arg neg-party))
                     args
                     domain-blames
                     domain-enforcers))
              (range-proc (apply val args*) neg-party)))))))))


(define ->contract
  (mlambda (x)
    (match x
      ((? contract?) x)
      ((? procedure?)
       (make-contract
        #:name (format #f "~s" (or (procedure-name x) '???))
        #:checker x))
      ((or (? symbol?)
           (? boolean?)
           (? keyword?))
       (make-contract
        #:name (format #f "~s" x)
        #:checker (cut eq? x <>)))
      ((or (? string?)
           (? bytevector?)
           (? char?)
           +nan.0)
       (make-contract
        #:name (format #f "~s" x)
        #:checker (cut equal? x <>)))
      ((? number?)
       (make-contract
        #:name (format #f "~s" x)
        #:checker (cut = x <>)))
      (_ (error (format #f "cannot use as contract: ~s" x))))))
