#lang s-exp syntax/module-reader
(planet jmillikan/asdl/language)

;; Using planet with a development link, just because the tutorial does.

#:read my-read
#:read-syntax my-read-syntax
 
(require "../parser.rkt")
 
(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))
 
(define (my-read-syntax src in)
  (parse src in))
