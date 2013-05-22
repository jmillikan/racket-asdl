#lang racket

;; See ASDL whitepaper (http://www.cs.princeton.edu/research/techreps/TR-554-97) for general ideas - syntax is dated.
;; See ASDL repository including manual for more details.

#| 

ASTs are exposed as syntax object wrapping S-expressions, with the properties following the module name and constructor name:

(Num 1) -> #'(Arith Num 1)

For now, explicit source location information in the AST is not tied to that in the syntax objects.

TODO (later): 
Some properties will have known "magic" names and types corresponding to syntax object properties e.g. (int lineno) and (int col_offset) in python.asdl.
Most ways of constructing ASTs and AST trees should duplicate these fields into corresponding syntax object fields.

TODO: Verify that all module, type and constructor names are unique (and not one of the four built-in names)
TODO: Checked constuctors
TODO: Syntax-case wrapper
TODO: quasisyntax shortcuts
TODO: Wrap built-in types as syntax objects in constructors

|#

(provide define-asdl-module)

(require (for-syntax syntax/strip-context))

;; Define a related set of ASDL types
(define-syntax (define-asdl-module stx)
  (syntax-case stx ()
    [(_ name types ...)
     ;; TODO: Move internal name generation out here...
     #'(begin 
         (define-asdl-type name name types) ...)]))

(define-for-syntax (postfix-in stx postfix context-stx) 
  (datum->syntax
   context-stx
   (string->symbol
    (string-append
     (symbol->string (syntax->datum stx))
     postfix))))

;; Natural language overload
;; ASDL constructors != racket constructors
;; However, they map 1-1 here right now.

;; def-stx is a syntax object to use for "context" in defining globals and provides.
;; TODO: Make definition-stx optional and test this standalone...
(define-syntax (define-asdl-type stx)
  (syntax-case stx ()
    [(_ def-stx ast-name (type-name (type-attributes ...) (ctr-names ctr-attributes ...) ...))
     (with-syntax ((type? (postfix-in #'type-name "?" #'def-stx))
                   (internal-type-name (strip-context #'type-name))
                   (type-case (postfix-in #'type-name "-case" #'def-stx))
                   ((ctr?s ...) 
                    (map (lambda (n) (postfix-in n "?" (syntax def-stx)))
                         (syntax-e #'(ctr-names ...))))
                   ((internal-ctr?s ...)
                    (map (lambda (n) (postfix-in n "?" #f))
                         (syntax-e #'(ctr-names ...))))
                   ((internal-ctr-names ...)
                    (map strip-context (syntax-e #'(ctr-names ...))))
                   ((syntax-pattern-ctr-names ...)
                    (map (lambda (n) (postfix-in n "-case" #'def-stx))
                         (syntax-e #'(ctr-names ...)))))
       #'(begin
           ;; Prefab to support match and syntax-case constructs
           (define-asdl-ctr ctr-names ctr?s internal-ctr-names syntax-pattern-ctr-names internal-type-name ast-name
             (ctr-attributes ...)
             (type-attributes ...)) ...
             
           (define (type? v) 
             (and (syntax? v)
                  (or (ctr?s v) ...)))
           ))]))

;; These are probably bad and wrong. TODO: Worry about this.
;; TODO: Get maybe-star out of here
(define-for-syntax (type-predicate stx maybe-star context)
  (let ((star? (syntax-case maybe-star (*)
                   [(*) #t]
                   [() #f]))
        (stx-pred (syntax-case stx (int object identifier string)
                    [int #'integer?]
                    [object #'(lambda _ #t)]
                    [identifier #'string?]
                    [string #'string?]
                    [other (postfix-in stx "?" context)])))
    (if star?
        (with-syntax ((stx? stx-pred))
          #'(lambda (v) (and (list? v)
                             (andmap stx? v))))
        stx-pred)))

;; Defines a racket constructor named ctr-name, a predicate for internal-ctr-name, and a syntax pattern macro name syntax-pattern-ctr-name

;; Name fail in here - constructor attributes are called props, type attributes are called attrs
(define-syntax (define-asdl-ctr stx)
  (syntax-case stx ()
    [(_ ctr-name ctr?-name internal-ctr-name syntax-pattern-ctr-name internal-type-name module-name
        ((prop-types prop-names maybe-star ...) ...)
        ((attr-types attr-names attr-maybe-star ...) ...))
     (with-syntax ((ctr-sym (strip-context #'ctr-name)) ;; >_<
                   ;; Really should be getting these next two from caller or *something*
                   ((prop-types? ...)
                    (map (lambda (n maybe-star) (type-predicate n maybe-star #'ctr?-name))
                         (syntax-e #'(prop-types ...))
                         (syntax-e #'((maybe-star ...) ...))))
                   ((attr-types? ...)
                    (map (lambda (n maybe-star) (type-predicate n maybe-star #'ctr?-name))
                         (syntax-e #'(attr-types ...))
                         (syntax-e #'((attr-maybe-star ...) ...)))))
     #'(begin
         (define (ctr?-name v)
           (and (syntax? v)
                (syntax-case v (ctr-sym)
                  [(module-name ctr-sym prop-names ... attr-names ...) #t]
                  [_ #f])))

         (define (ctr-name prop-names ... attr-names ...)
           (unless
               (and (prop-types? prop-names) ...
                    (attr-types? attr-names) ...)
             (error "Parameter type fail")) ;; TODO
           (datum->syntax #f (list 'module-name 'ctr-sym prop-names ... attr-names ...)))))]))

(module+ test
         (require rackunit)

         (define-asdl-module Arith
           (stm ((int lineno)) ;; attributes
                ;; 1+ constructors of (name args ...)
                (Compound (stm head) (stm next))
                (Assign (identifier id) (exp exp))
                (Print (exp args *)))
           (exp ()
                (Id (identifier id))
                (Num (int v))
                (Op (exp lval) (binop bop) (exp rval)))
           (binop ()
                  (Plus) 
                  (Minus) 
                  (Times) 
                  (Div)))

         ;; Creates...
         
         ;; Checked (TODO) constructors
         ;; Compound : stm * stm -> stm
         ;; Assign : identifier * exp -> stm
         ;; ...
         
         ;; predicates for left hands, constructors, and built-in types - mostly for testing and building custom handling
         ;; stm?
         ;; exp?
         ;; ...
         ;; Compound? 
         ;; Assign?
         ;; ...
         ;; asdl-int? (TODO)
         ;; ...

         (check-false (stm? 1))
         (check-false (Compound? 1))
         (check-false (Num? 1))

         ;; Checked constructors
         (check-exn exn:fail? (lambda () (Compound (Num 1) (Num 2) 1)))
         (check-exn exn:fail? (lambda () (Print (list (Plus)) 1)))
         (check-exn exn:fail? (lambda () (Num "won")))

         (check-true (exp? (Num 1)))
         (check-true (Num? (Num 1)))

         (check-true (Print? (Print (list) 12)))

         (check-false (Plus? (Minus)))

         ;; depickle : port -> AST
         ;; pickle : port * AST -> void
         
         ;; TODO

         ;; Support some kind of match, hopefully on top of syntax-case
         ;; (require (submod "arith.rkt" pattern))
         ;; (stm-case some-stm ()
         ;;   [(Compound head next) 
         ;;   ...

         ;; TODO:
         
         ;; Provides some kind of syntax-case creation, probably by a quasisyntax[/loc] wrapper like
         ;; (require (submod "arith.rkt" syntax))
         ;; (syntax-case maybe-some-ragg ()
         ;;   [(a "," rest ...) (Compound #:src maybe-some-ragg ,(something a) ,(something rest))]
         ;;   ...

         ;; TODO

)
