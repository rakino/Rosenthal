;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

;;; https://github.com/racket/racket/blob/master/racket/collects/racket/contract/private/blame.rkt
;;; Copyright © 2010-2019, 2021, 2024 Racket authors
;;; Ported from Racket, original copyright notice below:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to
;;; deal in the Software without restriction, including without limitation the
;;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;; IN THE SOFTWARE.

(define-module (rosenthal utils contract blame)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:export (blame?
            make-blame
            blame-source
            blame-positive
            blame-negative
            blame-contract
            blame-value
            blame-original?
            blame-swapped?
            blame-swap
            blame-replace-negative      ;used for indy blame
            blame-replaced-negative?    ;used for indy blame
            blame-update                ;used for option contract transfers
            blame-add-context
            ;; blame-add-unknown-context
            blame-context
            blame-replaced-negative?

            blame-add-missing-party
            blame-missing-party?
            ;; blame-add-extra-field

            raise-blame-error
            ;; current-blame-format
            ;; exn:fail:contract:blame
            ;; blame-fmt->-string

            ;; invariant-assertion-party

            ;; NOTE: New interfaces.
            contract-violation?
            make-blame*))

#|

Blame provides error reporting for the contract system.  This module implements
its structure and APIs, and currently follows Racket error message conventions.
See also:
https://docs.racket-lang.org/reference/exns.html#(part._err-msg-conventions)


Blame has a linked structure of <blame> -> <blame> -> ... -> <all-the-info>.

<all-the-info> records contract information and each <blame> node adds extra
context to it.  The extra context is combinator-specific (e.g. "the #1 element
of").  For higher-order functions, it also includes whether the positive party
and negative party are swapped.


The positive party provides a definition, and the negative party uses it.  For
contracts on higher-order functions, both parties are swapped for function
arguments.

For example, consider the following definition:

(define/contract (example f)
  (->/c (->/c number? number?) number?)
  ...)

The positive party and negative party for ‘example’ will be ‘example’ and
‘caller’, respectively.

‘f’ has a function contract ‘(->/c number? number?)’.  As an argument of
‘example’, it initially gets ‘example’ for positive and ‘caller’ for negative.

However the definition of ‘f’ is provided by the caller, so both parties should
be swapped: ‘caller’ for positive, and ‘example’ for negative.

|##|

‘make-blame’ constructs a blame.  (<blame> -> <all-the-info>)

(define b1
  (make-blame*
   #:source #f
   #:value 'my-function
   #:build-name (const "(listof/c (listof/c number?))")
   #:positive 'my-function
   #:negative 'caller
   #:original? #t))

b1
=> #<<blame>
     context-frame: #f
     swapped?: #f
     and-more: #<<all-the-info> ...>
(blame-swapped? b1)
=> #f
(blame-positive b1)
=> 'my-function
(blame-negative b1)
=> 'caller
(blame-context b1)
=> '()
(blame-contract b1)
=> "(listof/c (listof/c number?))"

‘blame-add-context’ adds a new head node with extra context. (<blame> -> ...)

(define b2
  (blame-add-context b1 "an element of" #:swap? #t))

b2
=> #<<blame>
     context-frame: "an element of"
     swapped? #t
     and-more: #<<blame>
                 context-frame: #f
                 swapped?: #f
                 and-more: #<<all-the-info> ...>>
(blame-swapped? b2)
=> #t
(blame-positive b2)
=> 'caller
(blame-negative b2)
=> 'my-function
(blame-context b2)
=> '("an element of")
(blame-contract b2)
=> "(listof/c (listof/c number?))"

(define b3
  (blame-add-context b2 "the #1 element of" #:swap? #t))

b3
=> #<<blame>
     context-frame: "the #1 element of"
     swapped? #t
     and-more: #<<blame>
                 context-frame: "an element of"
                 swapped? #t
                 and-more: #<<blame>
                             context-frame: #f
                             swapped?: #f
                             and-more: #<<all-the-info> ...>>
(blame-swapped? b3)
=> #f
(blame-positive b3)
=> 'my-function
(blame-negative b3)
=> 'caller
(blame-context b3)
=> '("the #1 element of" "an element of")
(blame-contract b3)
=> "(listof/c (listof/c number?))"

|#



(define-condition-type &contract-violation &message
  contract-violation?)

(define-record-type <important>
  (make-important name sense-swapped?)
  important?
  (name           important-name)            ; any/c
  (sense-swapped? important-sense-swapped?)) ; boolean?

(define-record-type <all-the-info>
  (make-all-the-info positive negative source value build-name important
                     missing-party? context-limit replaced-negative?)
  all-the-info?
  (positive           all-the-info-positive)            ; (or/c #f (listof/c any/c))
  (negative           all-the-info-negative)            ; (or/c #f (listof/c any/c))
  (source             all-the-info-source)              ; (or/c #f string?)
  (value              all-the-info-value)               ; any/c
  (build-name         all-the-info-build-name)          ; (->/c string?)
  (important          all-the-info-important)           ; important?
  (missing-party?     all-the-info-missing-party?)      ; boolean?
  (context-limit      all-the-info-context-limit)       ; (or/c #f (and/c integer? (not/c negative?)))
  (replaced-negative? all-the-info-replaced-negative?)) ; boolean?

(define (all-the-info-replace-positive all-the-info new-pos)
  (make-all-the-info
   (all-the-info-positive new-pos)
   (all-the-info-negative all-the-info)
   (all-the-info-source all-the-info)
   (all-the-info-value all-the-info)
   (all-the-info-build-name all-the-info)
   (all-the-info-important all-the-info)
   (all-the-info-missing-party? all-the-info)
   (all-the-info-context-limit all-the-info)
   (all-the-info-replaced-negative? all-the-info)))

(define (all-the-info-replace-negative all-the-info new-neg)
  (make-all-the-info
   (all-the-info-positive all-the-info)
   (all-the-info-negative new-neg)
   (all-the-info-source all-the-info)
   (all-the-info-value all-the-info)
   (all-the-info-build-name all-the-info)
   (all-the-info-important all-the-info)
   (all-the-info-missing-party? all-the-info)
   (all-the-info-context-limit all-the-info)
   (all-the-info-replaced-negative? all-the-info)))

(define-record-type <blame>
  (%make-blame context-frame swapped? and-more)
  blame?
  (context-frame %blame-context-frame)  ; (or/c #f string?)
  (swapped?      %blame-swapped?)       ; boolean?
  (and-more      %blame-and-more))      ; (or/c blame? all-the-info?)

(define* (make-blame source value build-name positive negative original?
                     #:key (context-limit #f))
  (define build/memo-name
    (lambda ()
      (build-name)))

  (define all-the-info
    (make-all-the-info
     (and positive (list positive))
     (and negative (list negative))
     source
     value
     build/memo-name
     #f
     (not (and positive negative))
     context-limit
     #f))

  (%make-blame
   #f
   (not original?)
   all-the-info))

(define* (make-blame* #:key source value build-name positive negative original?
                      (context-limit #f))
  (make-blame source value build-name positive negative original?
              #:context-limit context-limit))

(define (blame->all-the-info blame)
  (let loop ((b blame))
    (if (blame? b)
        (loop (%blame-and-more b))
        b)))

(define blame-source
  (compose all-the-info-source blame->all-the-info))
(define blame-value
  (compose all-the-info-value blame->all-the-info))
(define (blame-contract blame)
  ((all-the-info-build-name (blame->all-the-info blame))))
(define blame-important
  (compose all-the-info-important blame->all-the-info))
(define blame-missing-party?
  (compose all-the-info-missing-party? blame->all-the-info))
(define blame-context-limit
  (compose all-the-info-context-limit blame->all-the-info))
(define blame-replaced-negative?
  (compose all-the-info-replaced-negative? blame->all-the-info))

(define (blame-get-info blame getter)
  (let loop ((b blame)
             (swapped? #f))
    (if (blame? b)
        (loop (%blame-and-more b)
              (if (%blame-swapped? b)
                  (not swapped?)
                  swapped?))
        (getter b swapped?))))

(define (blame-set-info blame setter)
  (let loop ((b blame)
             (swap? #f))
    (if (blame? b)
        (%make-blame
         (%blame-context-frame b)
         (%blame-swapped? b)
         (loop (%blame-and-more b)
               (if (%blame-swapped? b)
                   (not swap?)
                   swap?)))
        (setter b swap?))))

(define (%blame-positive blame)
  (blame-get-info
   blame
   (lambda (all-the-info swapped?)
     (if swapped?
         (all-the-info-negative all-the-info)
         (all-the-info-positive all-the-info)))))

(define (%blame-negative blame)
  (blame-get-info
   blame
   (lambda (all-the-info swapped?)
     (if swapped?
         (all-the-info-positive all-the-info)
         (all-the-info-negative all-the-info)))))

(define (show-blame accessor blame)
  (match (accessor blame)
    ((x) x)
    (x x)))
(define blame-positive
  (cut show-blame %blame-positive <>))
(define blame-negative
  (cut show-blame %blame-negative <>))

(define (ensure-blame-known who blame)
  (unless (and (%blame-positive blame)
               (%blame-negative blame))
    (error who "blame info is not known; positive ~s negative ~s"
           (%blame-positive blame)
           (%blame-negative blame))))

(define (blame-update blame extra-positive extra-negative)
  (ensure-blame-known 'blame-update blame)
  (blame-set-info
   blame
   (lambda (all-the-info swap?)
     (if swap?
         (make-all-the-info
          (cons extra-negative
                (all-the-info-positive all-the-info))
          (cons extra-positive
                (all-the-info-negative all-the-info))
          (all-the-info-source all-the-info)
          (all-the-info-value all-the-info)
          (all-the-info-build-name all-the-info)
          (all-the-info-important all-the-info)
          (all-the-info-missing-party? all-the-info)
          (all-the-info-context-limit all-the-info)
          (all-the-info-replaced-negative? all-the-info))
         (make-all-the-info
          (cons extra-positive
                (all-the-info-positive all-the-info))
          (cons extra-negative
                (all-the-info-negative all-the-info))
          (all-the-info-source all-the-info)
          (all-the-info-value all-the-info)
          (all-the-info-build-name all-the-info)
          (all-the-info-important all-the-info)
          (all-the-info-missing-party? all-the-info)
          (all-the-info-context-limit all-the-info)
          (all-the-info-replaced-negative? all-the-info))))))

(define (blame-swap blame)
  (%make-blame
   (%blame-context-frame blame)
   (not (%blame-swapped? blame))
   (%blame-and-more blame)))

(define (blame-swapped? blame)
  (blame-get-info
   blame
   (lambda (_ swapped?) swapped?)))

(define blame-original?
  (negate blame-swapped?))

(define (blame/important-original? blame)
  (let ((important (blame-important blame)))
    (cond
     ((equal? (%blame-positive blame)
              (%blame-negative blame))
      #f)
     (important
      (equal? (important-sense-swapped? important)
              (blame-original? blame)))
     (else
      (blame-original? blame)))))

(define (blame-replace-negative blame new-neg)
  (blame-set-info
   blame
   (lambda (all-the-info swap?)
     (if swap?
         (all-the-info-replace-positive all-the-info new-neg)
         (all-the-info-replace-negative all-the-info new-neg)))))

(define (blame-add-missing-party blame missing-party)
  (if missing-party
      (begin
        (unless (blame-missing-party? blame)
          (error 'blame-add-missing-party "already have the party: ~s; trying to add ~s"
                 (if (blame-swapped? blame)
                     (%blame-positive blame)
                     (%blame-negative blame))
                 missing-party))
        (blame-set-info
         blame
         (lambda (all-the-info _)
           (make-all-the-info
            (or (all-the-info-positive all-the-info)
                (list missing-party))
            (or (all-the-info-negative all-the-info)
                (list missing-party))
            (all-the-info-source all-the-info)
            (all-the-info-value all-the-info)
            (all-the-info-build-name all-the-info)
            (all-the-info-important all-the-info)
            #f
            (all-the-info-context-limit all-the-info)
            (all-the-info-replaced-negative? all-the-info)))))
      blame))

(define (blame-context blame)
  (let loop ((top (%blame-context-frame blame))
             (b   (%blame-and-more blame)))
    (cond
     ((all-the-info? b)
      (if top
          (list top)
          '()))
     (else
      (cons top
            (loop (%blame-context-frame b)
                  (%blame-and-more b)))))))

(define* (blame-add-context blame context #:key important swap?)
  (let ((context-limit (blame-context-limit blame)))
    (if context-limit
        (cond
         ((not (zero? context-limit))
          (let ((limited-blame
                 dropped-swap?
                 (drop-to-limit blame context-limit)))
            (%make-blame
             context
             (not (equal? dropped-swap? swap?))
             limited-blame)))
         (swap?
          (%make-blame
           (%blame-context-frame blame)
           (not (%blame-swapped? blame))
           (%blame-and-more blame)))
         (else blame))
        (%make-blame
         context
         swap?
         (if important
             (let loop ((b blame))
               (if (blame? b)
                   (%make-blame
                    (%blame-context-frame b)
                    (%blame-swapped? b)
                    (loop (%blame-and-more b)))
                   (make-all-the-info
                    (all-the-info-positive b)
                    (all-the-info-negative b)
                    (all-the-info-source b)
                    (all-the-info-value b)
                    (all-the-info-build-name b)
                    (make-important
                     important
                     (if swap?
                         (not (blame-original? b))
                         (blame-original? b)))
                    (all-the-info-missing-party? b)
                    (all-the-info-context-limit b)
                    (all-the-info-replaced-negative? b))))
             blame)))))

(define (drop-to-limit blame context-limit)
  (define short-enough?
    (let loop ((b blame)
               (n (- context-limit 1)))
      (or (all-the-info? b)
          (if (zero? n)
              #f
              (loop (%blame-and-more b)
                    (- n 1))))))

  (if short-enough?
      (values blame #f)
      (let ()
        (define swapped? #f)
        (define limited-blame
          (let outer-loop ((outer-b blame)
                           (n (- context-limit 1)))
            (if (zero? n)
                (let inner-loop ((inner-b outer-b)
                                 (swap? #f))
                  (if (blame? inner-b)
                      (inner-loop (%blame-and-more inner-b)
                                  (if (%blame-swapped? inner-b)
                                      (not swap?)
                                      swap?))
                      (begin
                        (set! swapped? swap?)
                        inner-b)))
                (%make-blame
                 (%blame-context-frame outer-b)
                 (%blame-swapped? outer-b)
                 (outer-loop (%blame-and-more outer-b)
                             (- n 1))))))
        (values limited-blame swapped?))))

(define* (raise-blame-error raw-blame #:optional x #:key (missing-party #f)
                            #:rest fmt+args)
  (define blame
    (if (and (blame-original? raw-blame)
             (not missing-party))
        raw-blame
        (blame-add-missing-party raw-blame missing-party)))

  (define start-of-message
    (let ((self-or-not
           (if (blame/important-original? blame)
               "broke its own contract"
               "contract violation")))
      (cond
       ((blame-important blame)
        (format #f "~a: ~a" (important-name (blame-important blame)) self-or-not))
       ((blame-value blame)
        (format #f "~a: ~a" (blame-value blame) self-or-not))
       (else
        (format #f "~a" self-or-not)))))

  (define custom-message
    (let loop ((args fmt+args))
      (match args
        ;; Keyword arguments will be added to #:rest as well, strip them.
        (((? keyword? kw) arg . rest)
         (loop rest))
        ((fmt . fmt-args)
         ;; When formatting outputs, please use ~s to mimic Racket ~e.
         ;;
         ;; XXX: ~s doesn't directly map to Racket ~e.  With symbol 'x, ~s
         ;; gives ‘x’ but Racket ~e gives ‘'x’, for example.
         (apply format #f (blame-fmt->string blame fmt) fmt-args)))))

  (define context-lines
    (let ((context (blame-context blame)))
      (or (and context
               (not (null? context))
               (let loop ((ctx context)
                          (idx 0)
                          (res '()))
                 (if (null? ctx)
                     (reverse res)
                     (loop (cdr ctx)
                           (1+ idx)
                           (cons (if (zero? idx)
                                     (format #f "  in: ~a~%" (car ctx))
                                     (format #f "      ~a~%" (car ctx)))
                                 res)))))
          '())))

  (define contract-line
    (format #f "  ~a ~a~%"
            (if (null? context-lines) "in:" "   ")
            (blame-contract blame)))

  (define from-line
    (format #f "  contract from: ~a~%"
            (if (blame-original? blame)
                (last (%blame-positive blame))
                (last (%blame-negative blame)))))

  (define on-line
    (or (and (blame-important blame)
             (blame-value blame)
             (format #f "  contract on: ~a~%" (blame-value blame)))
        ""))

  (define blaming-line
    (let ((blame-parties (%blame-positive blame)))
      (match blame-parties
        ((one-party)
         (format #f "  blaming: ~a~%" one-party))
        ((multiple-parties ...)
         (format #f "  blaming multiple parties: ~{~a~%~}~%" multiple-parties)))))

  (define assumption-line
    "   (assuming the contract is correct)")

  (define at-line
    (or (and=> (blame-source blame)
               (cut format #f "~%  at: ~a" <>))
        ""))

  (raise
   (condition
    (&contract-violation
     (message
      (format #f "~%~a~a~{~a~}~a~a~a~a~a~a"
              start-of-message
              (if (string-null? custom-message) "~%" custom-message)
              context-lines
              contract-line
              from-line
              on-line
              blaming-line
              assumption-line
              at-line))))))

(define (blame-fmt->string blame fmt)
  (define original?
    (blame/important-original? blame))

  (define (tag->str+newline? key val)
    (define* (kv->str key val)
      (string-append key " " val))

    (match key
      ;; Racket doesn't use ‘got’, but ‘expected’ vs ‘got’ sounds more natural
      ;; considering directions.
      ((or 'given: 'got:)
       (values (kv->str (if original? "produced:" "given:") val)
               #t))
      ((or 'given 'got)
       (values (kv->str (if original? "produced" "given") val)
               #f))
      ('expected:
       (values (kv->str (if original? "promised:" "expected:") val)
               #t))
      ('expected
       (values (kv->str (if original? "promised" "expected") val)
               #f))
      ('received:
       (values (kv->str (if original? "supplied:" "received:") val)
               #t))))

  (define (normalize-continued-error-message str)
    "If STR contains newline (unless at the end), ensure there's a space next to
it."
    (define (not-space? x)
      (not (equal? x #\space)))

    (define str-newline-normalized
      (string-replace-substring str "~%" "\n"))

    (if (string-contains str-newline-normalized "\n")
        (let loop ((chars (string->list str-newline-normalized))
                   (result '()))
          (if (null? chars)
              (list->string (reverse result))
              (match chars
                ((#\newline (? not-space? next) . rest)
                 (loop rest
                       (append (reverse (list #\newline #\space next))
                               result)))
                ((first . rest)
                 (loop rest
                       (cons first result))))))
        str-newline-normalized))

  (define (build-string old new newline?)
    (if newline?
        (string-append old "\n  " new)
        (let ((old (if (string-null? old) ";\n" old))
              (new (normalize-continued-error-message new)))
          (if (or (string-suffix? " " old)
                  (string-match "^[\n ]" new))
              (string-append old new)
              (string-append old " " new)))))

  (if (string? fmt)
      fmt
      (let loop ((strs fmt)
                 (result ""))
        (if (null? strs)
            (if (string-null? result)
                result
                (string-append result "\n"))
            (match strs
              (((? symbol? key) (? string? val) . rest)
               (let ((str newline? (tag->str+newline? key val)))
                 (loop rest
                       (build-string result str newline?))))
              ((str . rest)
               (loop rest
                     (build-string result str #f))))))))
