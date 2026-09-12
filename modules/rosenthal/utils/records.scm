;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2012-2026 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2026 coopi <coldsideofyourpillow@disroot.org>

(define-module (rosenthal utils records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:autoload   (system syntax) (syntax-local-binding)
  #:export (define-record-type/dolly
            this-record

            match-record/dolly
            match-record-lambda/dolly))

(define-syntax record-error
  (syntax-rules ()
    "Report a syntactic error in use of CONSTRUCTOR."
    ((_ constructor form fmt args ...)
     (syntax-violation constructor
                       (format #f fmt args ...)
                       form))))

(eval-when (expand load eval)
  ;; The procedures below are needed both at run time and at expansion time.

  (define %record-constructor-max-argument-count
    ;; Maximum number of positional arguments for record constructors.  This
    ;; matches Guile's 'make-record-type', which uses a rest argument above
    ;; that number.
    20)

  (define (current-abi-identifier type)
    "Return an identifier unhygienically derived from TYPE for use as its
\"current ABI\" variable."
    (let ((type-name (syntax->datum type)))
      (datum->syntax
       type
       (string->symbol
        (string-append "% " (symbol->string type-name)
                       " abi-cookie")))))

  (define (record-abi-mismatch-error type)
    (throw 'record-abi-mismatch-error 'abi-check
           "~a: record ABI mismatch; recompilation needed"
           (list type) '()))

  (define (abi-check type cookie)
    "Return syntax that checks that the current \"application binary
interface\" (ABI) for TYPE is equal to COOKIE."
    (with-syntax ((current-abi (current-abi-identifier type)))
      #`(unless (eq? current-abi #,cookie)
          ;; The source file where this exception is thrown must be
          ;; recompiled.
          (record-abi-mismatch-error #,type))))

  (define* (report-invalid-field-specifier name bindings
                                           #:optional parent-form)
    "Report the first invalid binding among BINDINGS.  PARENT-FORM is used for
error-reporting purposes."
    (let loop ((bindings bindings))
      (syntax-case bindings ()
        (((field value) rest ...)                   ;good
         (loop #'(rest ...)))
        ((weird _ ...)                              ;weird!
         ;; WEIRD may be an identifier, thus lacking source location info, and
         ;; BINDINGS is a list, also lacking source location info.  Hopefully
         ;; PARENT-FORM provides source location info.
         (apply syntax-violation name "invalid field specifier"
                (if parent-form
                    (list parent-form #'weird)
                    (list #'weird)))))))

  (define (report-duplicate-field-specifier name ctor)
    "Report the first duplicate identifier among the bindings in CTOR."
    (syntax-case ctor ()
      ((_ bindings ...)
       (let loop ((bindings #'(bindings ...))
                  (seen   '()))
         (syntax-case bindings ()
           (((field value) rest ...)
            (not (memq (syntax->datum #'field) seen))
            (loop #'(rest ...) (cons (syntax->datum #'field) seen)))
           ((duplicate rest ...)
            (syntax-violation name "duplicate field initializer"
                              #'duplicate))
           (()
            #t)))))))

(define-syntax map-fields
  (lambda (x)
    (syntax-case x ()
      ((_ type within)
       (syntax-violation (syntax->datum #'within)
                         "undefined record type"
                         #'type))
      (_ (syntax-violation 'map-fields "bad use of syntactic keyword" x x)))))

(define-syntax-parameter this-record
  (lambda (s)
    "Return the record being defined.  This macro may only be used in the
context of the definition of a thunked field."
    (syntax-case s ()
      (id
       (identifier? #'id)
       (syntax-violation 'this-record
                         "cannot be used outside of a record instantiation"
                         #'id)))))

(define-syntax make-syntactic-constructor
  (syntax-rules ()
    "Make the syntactic constructor NAME for TYPE, that calls CTOR, and
expects all of EXPECTED fields to be initialized.  DEFAULTS is the list of
FIELD/DEFAULT-VALUE tuples, THUNKED is the list of identifiers of thunked
fields, DELAYED is the list of identifiers of delayed fields, and SANITIZERS
is the list of FIELD/SANITIZER tuples.

ABI-COOKIE is the cookie (an integer) against which to check the run-time ABI
of TYPE matches the expansion-time ABI."
    ((_ type name ctor (expected ...)
        #:abi-cookie abi-cookie
        #:thunked thunked
        #:this-identifier this-identifier
        #:delayed delayed
        #:innate innate
        #:sanitizers sanitizers
        #:defaults defaults)
     (define-syntax name
       (lambda (s)
         (define (constructor-call field-values)
           (if (< (length '(expected ...))
                  %record-constructor-max-argument-count)
               #`(ctor #,abi-cookie #,@field-values)
               #`(apply ctor #,abi-cookie (list #,@field-values))))

         (define (record-inheritance orig-record field+value)
           ;; Produce code that returns a record identical to ORIG-RECORD,
           ;; except that values for the FIELD+VALUE alist prevail.
           (define (field-inherited-value f)
             (and=> (find (lambda (x)
                            (eq? f (car (syntax->datum x))))
                          field+value)
                    car))

           ;; Make sure there are no unknown field names.
           (let* ((fields     (map (compose car syntax->datum) field+value))
                  (unexpected (lset-difference eq? fields '(expected ...))))
             (when (pair? unexpected)
               (record-error 'name s "extraneous field initializers ~a"
                             unexpected)))

           (constructor-call
            (map (lambda (field index)
                   (or (field-inherited-value field)
                       (if (innate-field? field)
                           (wrap-field-value
                            field (field-default-value field))
                           #`(struct-ref #,orig-record #,index))))
                 '(expected ...)
                 (iota (length '(expected ...))))))

         (define (thunked-field? f)
           (memq (syntax->datum f) 'thunked))

         (define (delayed-field? f)
           (memq (syntax->datum f) 'delayed))

         (define (innate-field? f)
           (memq (syntax->datum f) 'innate))

         (define field-sanitizer
           (let ((lst (map (match-lambda
                             ((f p)
                              (list (syntax->datum f) p)))
                           #'sanitizers)))
             (lambda (f)
               (or (and=> (assoc-ref lst (syntax->datum f)) car)
                   #'(lambda (x) x)))))

         (define (field-index f)
           ;; Return the index of F within the record.
           (let ((f (syntax->datum f)))
             (let loop ((fields '(expected ...))
                        (index 0))
               (match fields
                 (()
                  ;; Internal error.
                  (record-error 'name s "field not found ~a" f))
                 ((head . rest)
                  (if (eq? f head)
                      index
                      (loop rest (+ 1 index))))))))

         (define (check-shadowing identifier)
           ;; Warn if IDENTIFIER shadows a local binding.
           ;; Note: not using (guix diagnostics) to remain independent of
           ;; other Guix modules.
           (when (eq? 'lexical (syntax-local-binding identifier))
             (format (current-warning-port)
                     "~a: inherited field binding '~a' of \
record type '~a' shadows local variable~%"
                     (match (syntax-source identifier)
                       (#f "<unknown-location>")
                       (lst (format #f "~a:~a:~a"
                                    (assq-ref lst 'filename)
                                    (and=> (assq-ref lst 'line) 1+)
                                    (assq-ref lst 'column))))
                     (syntax->datum identifier)
                     (syntax->datum #'type))))

         (define* (wrap-field-value f value #:optional parent)
           ;; Wrap VALUE, the value of field F, such that its sanitizer is
           ;; called and its properties (thunked, delayed) honored.  When
           ;; PARENT is true, bind F to the value inherited from PARENT in the
           ;; lexical scope of VALUE.
           (let* ((sanitizer (field-sanitizer f))
                  (value     #`(#,sanitizer #,value)))
             (cond ((thunked-field? f)
                    (if parent
                        ;; Compute the value being inherited by calling the
                        ;; thunked field F of PARENT with a self-reference for
                        ;; the new record being constructed.
                        (with-syntax ((inherited
                                       #`((struct-ref #,parent
                                                      #,(field-index f))
                                          #,this-identifier)))
                          (check-shadowing f)
                          #`(lambda (x)
                              (syntax-parameterize ((#,this-identifier
                                                     (lambda (s)
                                                       (syntax-case s ()
                                                         (id
                                                          (identifier? #'id)
                                                          #'x)))))
                                ;; Bind F, the field identifier, to the value
                                ;; being inherited.
                                (let-syntax ((#,f (identifier-syntax inherited)))
                                  #,value))))
                        #`(lambda (x)
                            (syntax-parameterize ((#,this-identifier
                                                   (lambda (s)
                                                     (syntax-case s ()
                                                       (id
                                                        (identifier? #'id)
                                                        #'x)))))
                              #,value))))
                   ((delayed-field? f)
                    #`(delay #,value))
                   (else value))))

         (define default-values
           ;; List of symbol/value tuples.
           (map (match-lambda
                  ((f v)
                   (list (syntax->datum f) v)))
                #'defaults))

         (define (field-default-value f)
           (car (assoc-ref default-values (syntax->datum f))))

         (define (field-bindings field+value)
           ;; Return field to value bindings, for use in 'let*' below.
           (map (lambda (field+value)
                  (syntax-case field+value ()
                    ((field value)
                     #`(field
                        #,(wrap-field-value #'field #'value)))))
                field+value))

         (define (field-bindings/inheritance parent field+value)
           ;; Return field to value bindings, for use in 'let*' below.
           (map (lambda (field+value)
                  (syntax-case field+value ()
                    ((field value)
                     #`(field
                        #,(wrap-field-value #'field #'value parent)))))
                field+value))

         (syntax-case s (inherit expected ...)
           ((_ (inherit orig-record) (field value) (... ...))
            #`(let* #,(field-bindings/inheritance #'orig-record
                                                  #'((field value) (... ...)))
                #,(abi-check #'type abi-cookie)
                #,(record-inheritance #'orig-record
                                      #'((field value) (... ...)))))
           ((_ (field value) (... ...))
            (let ((fields (map syntax->datum #'(field (... ...)))))
              (define (field-value f)
                (or (find (lambda (x)
                            (eq? f (syntax->datum x)))
                          #'(field (... ...)))
                    (wrap-field-value f (field-default-value f))))

              ;; Pass S to make sure source location info is preserved.
              (report-duplicate-field-specifier 'name s)

              (let ((fields (append fields (map car default-values))))
                (cond ((lset= eq? fields '(expected ...))
                       #`(let* #,(field-bindings
                                  #'((field value) (... ...)))
                           #,(constructor-call
                              (map field-value '(expected ...)))))
                      ((pair? (lset-difference eq? fields
                                               '(expected ...)))
                       (record-error 'name s
                                     "extraneous field initializers ~a"
                                     (lset-difference eq? fields
                                                      '(expected ...))))
                      (else
                       (record-error 'name s
                                     "missing field initializers ~a"
                                     (lset-difference eq?
                                                      '(expected ...)
                                                      fields)))))))
           ((_ bindings (... ...))
            ;; One of BINDINGS doesn't match the (field value) pattern.
            ;; Report precisely which one is faulty, instead of letting the
            ;; "source expression failed to match any pattern" error.
            (report-invalid-field-specifier 'name
                                            #'(bindings (... ...))
                                            s))))))))

(define-syntax-rule (define-field-property-predicate predicate property)
  "Define PREDICATE as a procedure that takes a syntax object and, when passed
a field specification, returns the field name if it has the given PROPERTY."
  (define (predicate s)
    (syntax-case s (property)
      ((field (property values (... ...)) _ (... ...))
       #'field)
      ((field _ properties (... ...))
       (predicate #'(field properties (... ...))))
      (_ #f))))

(define-syntax define-record-type/dolly
  (lambda (s)
    "Define the given record type such that an additional \"syntactic
constructor\" is defined, which allows instances to be constructed with named
field initializers, à la SRFI-35, as well as default values.  An example use
may look like this:

  (define-record-type* <thing> thing make-thing
    thing?
    this-thing
    (name  thing-name (default \"chbouib\"))
    (port  thing-port
           (default (current-output-port)) (thunked))
    (loc   thing-location (innate) (default (current-source-location))))

This example defines a macro 'thing' that can be used to instantiate records
of this type:

  (thing
    (name \"foo\")
    (port (current-error-port)))

The value of 'name' or 'port' could as well be omitted, in which case the
default value specified in the 'define-record-type*' form is used:

  (thing)

The 'port' field is \"thunked\", meaning that calls like '(thing-port x)' will
actually compute the field's value in the current dynamic extent, which is
useful when referring to fluids in a field's value.  Furthermore, that thunk
can access the record it belongs to via the 'this-thing' identifier.

A field can also be marked as \"delayed\" instead of \"thunked\", in which
case its value is effectively wrapped in a (delay …) form.

A field can also have an associated \"sanitizer\", which is a procedure that
takes a user-supplied field value and returns a \"sanitized\" value for the
field:

  (define-record-type* <thing> thing make-thing
    thing?
    this-thing
    (name  thing-name
           (sanitize (lambda (value)
                       (cond ((string? value) value)
                             ((symbol? value) (symbol->string value))
                             (else (throw 'bad! value)))))))

It is possible to copy an object 'x' created with 'thing' like this:

  (thing (inherit x) (name \"bar\"))

This expression returns a new object equal to 'x' except for its 'name'
field and its 'loc' field---the latter is marked as \"innate\", so it is not
inherited."

    (define (rtd-identifier type)
      ;; Return an identifier derived from TYPE to name its record type
      ;; descriptor (RTD).
      (let ((type-name (syntax->datum type)))
        (datum->syntax
         type
         (string->symbol
          (string-append "% " (symbol->string type-name) " rtd")))))

    (define (field-default-value s)
      (syntax-case s (default)
        ((field (default val) _ ...)
         (list #'field #'val))
        ((field _ properties ...)
         (field-default-value #'(field properties ...)))
        (_ #f)))

    (define (field-sanitizer s)
      (syntax-case s (sanitize)
        ((field (sanitize proc) _ ...)
         (list #'field #'proc))
        ((field _ properties ...)
         (field-sanitizer #'(field properties ...)))
        (_ #f)))

    (define-field-property-predicate delayed-field? delayed)
    (define-field-property-predicate thunked-field? thunked)
    (define-field-property-predicate innate-field? innate)

    (define (wrapped-field? s)
      (or (thunked-field? s) (delayed-field? s)))

    (define (wrapped-field-accessor-name field)
      ;; Return the name (an unhygienic syntax object) of the "real"
      ;; getter for field, which is assumed to be a wrapped field.
      (syntax-case field ()
        ((field get properties ...)
         (let* ((getter      (syntax->datum #'get))
                (real-getter (symbol-append '% getter '-real)))
           (datum->syntax #'get real-getter)))))

    (define (field-spec->srfi-9 field)
      ;; Convert a field spec of our style to a SRFI-9 field spec of the
      ;; form (field get).
      (syntax-case field ()
        ((name get properties ...)
         #`(name
            #,(if (wrapped-field? field)
                  (wrapped-field-accessor-name field)
                  #'get)))))

    (define (thunked-field-accessor-definition field)
      ;; Return the real accessor for FIELD, which is assumed to be a
      ;; thunked field.
      (syntax-case field ()
        ((name get _ ...)
         (with-syntax ((real-get (wrapped-field-accessor-name field)))
           #'(define-inlinable (get x)
               ;; The real value of that field is a thunk, so call it.
               ((real-get x) x))))))

    (define (delayed-field-accessor-definition field)
      ;; Return the real accessor for FIELD, which is assumed to be a
      ;; delayed field.
      (syntax-case field ()
        ((name get _ ...)
         (with-syntax ((real-get (wrapped-field-accessor-name field)))
           #'(define-inlinable (get x)
               ;; The real value of that field is a promise, so force it.
               (force (real-get x)))))))

    (define (compute-abi-cookie field-specs)
      ;; Compute an "ABI cookie" for the given FIELD-SPECS.  We use
      ;; 'string-hash' because that's a better hash function that 'hash' on a
      ;; list of symbols.
      (syntax-case field-specs ()
        (((field get properties ...) ...)
         ;; Passing (target-most-positive-fixnum) as the second argument of
         ;; 'string-hash' won't have the intended effect when cross-compiling
         ;; because that second argument is used to compute a modulo after the
         ;; hash has been computed on an 'unsigned long'.  Instead, only keep
         ;; the 32 most significant bits on 64-bit platforms, unconditionally.
         ;; See <https://issues.guix.gnu.org/74296>.
         (let ((hash-value
                (string-hash
                 (object->string (syntax->datum #'((field properties ...) ...))))))
           (cond
            ((< most-positive-fixnum (ash 1 32)) hash-value)
            ((< most-positive-fixnum (ash 1 64)) (ash hash-value -32))
            (else (error "unexpected!" most-positive-fixnum)))))))

    (syntax-case s ()
      ((_ type syntactic-ctor ctor pred
          this-identifier
          (field get properties ...) ...)
       (identifier? #'this-identifier)
       (let* ((field-spec #'((field get properties ...) ...))
              (thunked    (filter-map thunked-field? field-spec))
              (delayed    (filter-map delayed-field? field-spec))
              (innate     (filter-map innate-field? field-spec))
              (defaults   (filter-map field-default-value
                                      #'((field properties ...) ...)))
              (sanitizers (filter-map field-sanitizer
                                      #'((field properties ...) ...)))
              (cookie     (compute-abi-cookie field-spec))
              (field-count (length field-spec))
              (constructor-procedure
               (datum->syntax
                #'ctor
                (symbol-append (string->symbol " %")
                               (syntax->datum #'ctor)
                               '-procedure/abi-check))))

         (define (constructor-body)
           (if (< field-count %record-constructor-max-argument-count)
               #`(case-lambda
                   ((actual-cookie field ...)
                    (unless (eq? actual-cookie #,cookie)
                      (record-abi-mismatch-error type))
                    (ctor field ...))
                   (_
                    (record-abi-mismatch-error type)))
               #`(case-lambda
                   ((actual-cookie . field-values)
                    (unless (and (eq? actual-cookie #,cookie)
                                 (= (length field-values)
                                    #,field-count))
                      (record-abi-mismatch-error type))
                    (apply ctor field-values))
                   (_
                    (record-abi-mismatch-error type)))))

         (with-syntax ((ctor-procedure constructor-procedure)
                       ((field-spec* ...)
                        (map field-spec->srfi-9 field-spec))
                       ((field-type ...)
                        (map (match-lambda
                               ((? thunked-field?)
                                (datum->syntax s 'thunked))
                               ((? delayed-field?)
                                (datum->syntax s 'delayed))
                               (else
                                (datum->syntax s 'normal)))
                             field-spec))
                       ((thunked-field-accessor ...)
                        (filter-map (lambda (field)
                                      (and (thunked-field? field)
                                           (thunked-field-accessor-definition
                                            field)))
                                    field-spec))
                       ((delayed-field-accessor ...)
                        (filter-map (lambda (field)
                                      (and (delayed-field? field)
                                           (delayed-field-accessor-definition
                                            field)))
                                    field-spec)))
           #`(begin
               (define-record-type #,(rtd-identifier #'type)
                 (ctor field ...)
                 pred
                 field-spec* ...)

               ;; Rectify the vtable type name...
               (set-struct-vtable-name! #,(rtd-identifier #'type) 'type)
               (cond-expand
                (guile-3
                 ;; ... and the record type name.
                 (struct-set! #,(rtd-identifier #'type) vtable-offset-user
                              'type))
                (else #f))

               (define-syntax type
                 (lambda (s)
                   "This macro lets us query record type info at
macro-expansion time."
                   (syntax-case s (map-fields)
                     ((_ (map-fields _ _) macro)
                      #'(macro ((field field-type) ...)))
                     (id
                      (identifier? #'id)
                      #'#,(rtd-identifier #'type)))))

               (define #,(current-abi-identifier #'type)
                 #,cookie)

               #,@(if (free-identifier=? #'this-identifier #'this-record)
                      #'()
                      #'((define-syntax-parameter this-identifier
                           (lambda (s)
                             "Return the record being defined.  This macro may
only be used in the context of the definition of a thunked field."
                             (syntax-case s ()
                               (id
                                (identifier? #'id)
                                (syntax-violation 'this-identifier
                                                  "cannot be used outside \
of a record instantiation"
                                                  #'id)))))))
               thunked-field-accessor ...
               delayed-field-accessor ...

               (define ctor-procedure
                 ;; This procedure is *not* inlined, to reduce code bloat
                 ;; (struct initialization takes at least one instruction per
                 ;; field).
                 #,(constructor-body))

               (make-syntactic-constructor type syntactic-ctor ctor-procedure
                                           (field ...)
                                           #:abi-cookie #,cookie
                                           #:thunked #,thunked
                                           #:this-identifier #'this-identifier
                                           #:delayed #,delayed
                                           #:innate #,innate
                                           #:sanitizers #,sanitizers
                                           #:defaults #,defaults)))))
      ((_ type syntactic-ctor ctor pred
          (field get properties ...) ...)
       ;; When no 'this' identifier was specified, use 'this-record'.
       #'(define-record-type/dolly type syntactic-ctor ctor pred
           this-record
           (field get properties ...) ...)))))


;;;
;;; Pattern matching.
;;;

(define-syntax lookup-field+wrapper
  (lambda (s)
    "Look up FIELD in the given list and return both an expression that represents
its offset in the record and a procedure that wraps it to return its \"true\" value
(for instance, FORCE is returned in the case of a delayed field).  RECORD is passed
to thunked values.  Raise a syntax violation when the field is not found."
    (syntax-case s (normal delayed thunked)
      ((_ record field offset ())
       (syntax-violation 'match-record
                         "unknown record type field"
                         ;; Attach the local source data to the field.
                         (datum->syntax #f (syntax->datum #'field) #:source s)))
      ((_ record field offset ((head normal) tail ...))
       (free-identifier=? #'field #'head)
       #'(values offset identity))
      ((_ record field offset ((head delayed) tail ...))
       (free-identifier=? #'field #'head)
       #'(values offset force))
      ((_ record field offset ((head thunked) tail ...))
       (free-identifier=? #'field #'head)
       #'(values offset (cut <> record)))
      ((_ record field offset (_ tail ...))
       #'(lookup-field+wrapper record field
                               (+ 1 offset) (tail ...))))))

(define-syntax match-record-inner
  (lambda (s)
    (syntax-case s ()
      ((_ record type ((field variable) rest ...) body ...)
       #'(let-syntax ((field-offset+wrapper
                       (syntax-rules ()
			 ((_ f)
                          (lookup-field+wrapper record field 0 f)))))
           (let*-values (((offset wrap)
                          (type (map-fields type match-record)
                                field-offset+wrapper))
                         ((variable)
                          (wrap (struct-ref record offset))))
             (match-record-inner record type (rest ...) body ...))))
      ((_ record type (field rest ...) body ...)
       ;; Redirect to the canonical form above.
       #'(match-record-inner record type ((field field) rest ...) body ...))
      ((_ record type () body ...)
       #'(begin body ...)))))

(define-syntax match-record/dolly
  (syntax-rules ()
    "Bind each FIELD of a RECORD of the given TYPE to its FIELD name.
The order in which fields appear does not matter.  A syntax error is raised if
an unknown field is queried."
    ((_ record type (fields ...) body ...)
     (if (eq? (struct-vtable record) type)
         (match-record-inner record type (fields ...) body ...)
         (throw 'wrong-type-arg "match-record"
                "Wrong type (expecting a ~a record): ~S"
                (list 'type record) (list record))))))

(define-syntax match-record-lambda/dolly
  (syntax-rules ()
    "Return a procedure accepting a single record of the given TYPE for which each
FIELD will be bound to its FIELD name within the returned procedure.  A syntax error
is raised if an unknown field is queried."
    ((_ type (field ...) body ...)
     (lambda (record)
       (match-record/dolly record type (field ...)
         body ...)))))
