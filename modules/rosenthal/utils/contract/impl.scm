;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils contract impl)
  #:use-module (srfi srfi-9)
  #:use-module (rosenthal utils contract blame)
  #:export (contract?
            make-contract
            contract-name
            contract-checker
            contract-enforcer
            compound-contract-name))

#|

This module provides the data representation of contracts.

When constructing a contract via ‘make-contract’, at least one of a checker or
an enforcer must be specified.

A checker is a predicate function.  An enforcer is a function that actually
performs the check and either returns the value or raises an error with extra
context.  If only a checker is provided, an enforcer will be automatically
created.

Note that checkers should only be provided when values can be checked
immediately (e.g. checking a value is a number).  Checkers don't work well for
things like a list of numbers, since we won't know the location and context of a
failure.

Contract combinators may rely on checkers' availability for optimizations.

Enforcers are "late neg projections" in Racket, which look like the following:

(lambda (blame)
  (lambda (val neg-party)
    ...))

This allows a contract to be partially applied when it's constructed and
attached, before the value is actually checked.

See also (rosenthal utils contract combinators) for more examples, and
(rosenthal utils contract combinators) for error reporting of contracts.

|#


(define-record-type <contract>
  (%make-contract name checker enforcer)
  contract?
  (name     contract-name)         ; string?
  (checker  contract-checker)      ; (or/c #f (->/c any/c boolean?))
  (enforcer contract-enforcer))    ; (->/c blame? (->/c any/c any/c any/c))

(define* (make-contract #:key name checker enforcer)
  (%make-contract
   name
   checker
   (or enforcer
       (lambda (blame)
         (lambda (val neg-party)
           (if (checker val)
               val
               (raise-blame-error blame #:missing-party neg-party
                                  `(expected: "~a" got: "~s") name val)))))))

(define (compound-contract-name name ctcs)
  (format #f "(~a ~a)"
          name
          (string-join (map contract-name ctcs) " ")))
