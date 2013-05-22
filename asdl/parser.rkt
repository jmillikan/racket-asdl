#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)

         (prefix-in grammar: "grammar.rkt")
         ragg/support
         syntax/stx)

;; For now, use the subset in Python.asdl (recent versions)

;; LEXER

(define (lex-string port real-start-pos)
  ;; unquote-count: <= quote-count
  ;; lexeme-lst: reversed list of lexemes including separate endquotes
  (define (continue-string port lexeme-lst)
    (define string-part-lex
      (lexer
       ("\""
        (let ((content-string (string-append* (reverse lexeme-lst))))
          (token 'STRING content-string
                 #:line (position-line real-start-pos)
                 #:column (position-col real-start-pos)
                 #:offset (position-offset real-start-pos)
                 #:span (- (position-offset end-pos) (position-offset real-start-pos)))))

       (any-char (continue-string port (cons lexeme lexeme-lst)))))
    (string-part-lex port))
  (continue-string port '()))

(define lex-comment
  (lexer
   (eol (lex input-port))
   (any-char (lex-comment input-port))))

(define-lex-abbrevs
  (eol (:or "\n" "\r\n" "\r"))

  (upper (char-range #\A #\Z))
  (lower (char-range #\a #\z))
  (alpha (:or upper lower "_"))
  (alpha-num (:or alpha (char-range "0" "9")))
  (typ-id (:: lower (:* alpha-num)))
  (con-id (:: upper (:* alpha-num))))

(define-syntax-rule (pos-token sym val)
  (token sym val 
         #:line (position-line start-pos)
         #:column (position-col start-pos)
         #:offset (position-offset start-pos)
         #:span (- (position-offset end-pos) (position-offset start-pos))))

(define lex 
  (lexer 
   ["--" (lex-comment input-port)]
   ["\"" (lex-string input-port start-pos)]
   [(:or "module" "attributes" "version")
    (pos-token (string->symbol lexeme) lexeme)]
   [(:or "{" "}" "(" "," ")" "=" "|" "*" "?")
    (pos-token (string->symbol lexeme) lexeme)]
   [typ-id (pos-token 'TYPEID lexeme)]
   [con-id (pos-token 'CONSTRUCTORID lexeme)]
   [(:+ (:or " " "\t" "\f" eol)) (lex input-port)]
   [(eof) (token 'EOF "EOF")]))

;; For sanity-checking lexer
(define (lex-list port)
  (local [(define (lex-acc port acc)
	   (let ((token (lex port)))
	     (if (equal? (token-struct-type token) 'EOF)
		 acc
		 (lex-acc port (cons token acc)))))]
	 (reverse (lex-acc port (list)))))

;; This is probably not right, but seems to work.
(define (compare-ids a b)
  (equal? (syntax-e a) (syntax-e b)))

(define (convert-names stx)
  (syntax-case stx () 
    [(name-or-list ...) (map convert-names (syntax-e #'(name-or-list ...)))]
    [name (string->symbol (syntax-e #'name))]))

;; Everything from this to the tests is as dumb as two boxes of rocks.

(define (destructure-parse stx)
  (syntax-case* stx (modules module type) compare-ids
                [(modules
                  (module "module" module-name-s _ "{" 
                    (type type-name "=" (constructor_list constructor ...) maybe-attributes) ... "}") ...)
                 (with-syntax ((((type-name ...) ...) (convert-names #'((type-name ...) ...)))
                               ((module-name ...) (convert-names #'(module-name-s ...)))
                               ((((constructor ...) ...) ...) (convert-constructors #'(((constructor ...) ...) ...)))
                               (((attributes ...) ...) (convert-attributes #'((maybe-attributes ...) ...))))
                   #'(begin 
                       (define-asdl-module module-name
                         (type-name attributes constructor ...) ...
                         ) ...))]))

(define (convert-constructors top-ctrs)
  (stx-map
   (lambda (module-ctrs)
     (stx-map
      (lambda (type-ctrs)
        (append*
         (stx-map 
          (lambda (ctr)
            (syntax-case* ctr (constructor) compare-ids
                          ["|" '()]
                          [(constructor _ ...)
                           (list (destructure-constructor ctr))]))
         type-ctrs)))
      module-ctrs))
   top-ctrs))

(define (convert-attributes top-attrs)
  (stx-map
   (lambda (module-attrs)
     (stx-map 
      (lambda (type-attrs)
        (syntax-case* type-attrs (maybe-attributes) compare-ids
                      [(maybe_attributes) #'()]
                      [(maybe_attributes "attributes" (properties "(" properties-and-commas ... ")"))
                       (convert-properties #'(properties-and-commas ...))]))
      module-attrs))
   top-attrs))
        
   
(define (destructure-constructor stx)
  (syntax-case* stx (constructor properties) compare-ids
                [(constructor ctr-name) 
                 (with-syntax ((ctr-name (convert-names #'ctr-name)))
                   #'(ctr-name))]
                [(constructor ctr-name 
                              (properties "(" properties-and-commas ... ")"))
                 (with-syntax ((ctr-name (convert-names #'ctr-name))
                               ((properties ...) (convert-properties #'(properties-and-commas ...))))
                 #'(ctr-name properties ...))]))

(define (convert-properties stx)
  (syntax-case stx () 
               [(property) (list (destructure-property #'property))]
               [(property "," . rest) (cons (destructure-property #'property) 
                                            (convert-properties #'rest))]
               [_ (error "Bad parse tree: convert-properties")]))

(define (destructure-property stx)
  (syntax-case* stx (property maybe-modifier) compare-ids
                [(property type (maybe_modifier) name) 
                 (with-syntax ((type (convert-names #'type))
                               (name (convert-names #'name)))
                 #'(type name))]
                [(property type (maybe_modifier mod) name)
                 (with-syntax ((type (convert-names #'type))
                               (name (convert-names #'name))
                               (mod (convert-names #'mod)))
                   #'(type name mod))]
                [_ (error "Bad parse tree: destructure-property")]))
  
(module+ test
         (require rackunit)

(define arith-src #<<END
module Arith
{
stm = Compound(stm head, stm next) 
    | Assign(identifier id, exp exp) 
    | Print(exp* args)
exp = Id(identifier id) 
    | Num(int v)
    | Op(exp lval, binop bop, exp rval) 
binop = Plus | Minus | Times | Div
}
END
)

(define arith-port (open-input-string arith-src))
(check-not-exn (lambda () (grammar:parse (lambda () (lex arith-port)))))

(define simple-src "module Simple { t = C | D(t a) }")

;; Sanity check lexer & grammar, also have a reference for the shape of the parse tree
(let ((simple-port (open-input-string simple-src)))
  (check-equal? (syntax->datum (grammar:parse (lambda () (lex simple-port))))
                '(modules (module "module" "Simple" (maybe_version) "{" 
                            (type "t" "=" 
                                  (constructor_list 
                                   (constructor "C") "|"
                                   (constructor "D"
                                                (properties 
                                                 "(" (property "t" (maybe_modifier) "a") ")")))
                                  (maybe_attributes))
                            "}"))))

(check-equal? (convert-names #'("Jim" "Sue")) '(Jim Sue))
(check-equal? (convert-names #'"Jim") 'Jim)
(check-equal? (convert-names #'(("Jim") "Sue")) '((Jim) Sue))

(let ((simple-port (open-input-string simple-src)))
  (check-equal? (syntax->datum (destructure-parse (grammar:parse (lambda () (lex simple-port)))))
                '(begin
                   (define-asdl-module Simple
                     (t ()
                        (C)
                        (D (t a)))))))

(let ((attr-port (open-input-string "module A { b = C attributes (int d) }")))
  (check-equal? (syntax->datum (destructure-parse (grammar:parse (lambda () (lex attr-port)))))
                '(begin
                   (define-asdl-module A
                     (b ((int d))
                        (C))))))
)
