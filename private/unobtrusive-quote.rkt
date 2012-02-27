#lang racket/base

(require racket/match
         "../ast.rkt")
(require (for-syntax racket/base))
(require (for-template racket/base))
;(provide (except-out (all-defined-out) define-quoter))
(provide except-out (all-defined-out)
         define-quoter syntax-quote-option syntax-quote-map)

#;(define (wrap quoter)
  (procedure-rename
   (lambda (x)
     (with-syntax ([ast (quoter x)])
       (syntax/loc #'ast
         (quasiquote ast))))
   (object-name quoter)))

(define-syntax (define-quoter stx)
  (syntax-case stx ()
    [(_ f @ [(struct-type ([field quote-field] ...)) template] ...)
     #'(define (f x [original? #t])
         (match x
           [(struct struct-type (src field ...))
            (let ([q-src (and src (src->syntax src #f original?))])
              (with-syntax ([@ (and src (syntax-quote-src src))]
                            [field (quote-field field)] ...)
                (if q-src
                    ;; XXX: major duplication-age
                    (syntax/loc q-src template)
                    (syntax template))))]
           ...))]))

(define ((syntax-quote-option f) x)
  (and x (f x)))

(define ((syntax-quote-map f) ls)
  (with-syntax ([(x ...) (map f ls)])
    #'(x ...)))

(define (syntax-quote-src x)
  (match x
    [(struct src (start-offset start-line start-col end-offset end-line end-col path))
     (with-syntax ([start-offset start-offset]
                   [start-line start-line]
                   [start-col start-col]
                   [end-offset end-offset]
                   [end-line end-line]
                   [end-col end-col]
                   [path path])
       #'#s(src start-offset start-line start-col end-offset end-line end-col path))]))

(define-quoter syntax-quote-expr @
  [(expr:ref ([id syntax-quote-id]))
   #s((expr:ref expr 1) @ id)]
  )

(define-quoter syntax-quote-decl @
  [(decl:vars ([class (syntax-quote-option syntax-quote-id)]
               [type (syntax-quote-option syntax-quote-type)]
               [declarators (syntax-quote-map syntax-quote-decl)]))
   #s((decl:vars decl 1) @ class type declarators)]
  [(decl:declarator ([id (syntax-quote-option syntax-quote-id)]
                     [type (syntax-quote-option syntax-quote-type)]
                     [init (syntax-quote-option syntax-quote-init)]))
   #s((decl:declarator decl 1) @ id type init)]
  [(decl:member ([type (syntax-quote-option syntax-quote-type)]
                 [declarators (syntax-quote-map syntax-quote-decl)]))
   #s((decl:member decl 1) @ type declarators)]
  [(decl:member-declarator ([id (syntax-quote-option syntax-quote-id)]
                            [type (syntax-quote-option syntax-quote-type)]
                            [init (syntax-quote-option syntax-quote-init)]
                            [bit-size (syntax-quote-option syntax-quote-expr)]))
   #s((decl:member-declarator decl 1) @ id type init bit-size)]
  )

(define-quoter syntax-quote-type @
  [(type:primitive ([name values]))
   #s((type:primitive type 1) @ name)]
  [(type:ref ([name values]))
   #s((type:ref type 1) @ name)]
  [(type:struct ([tag values]
                 [fields (syntax-quote-map syntax-quote-decl)]))
   #s((type:struct type 1) @ tag fields)]
  )

(define-quoter syntax-quote-init @
  [(init:expr ([expr syntax-quote-expr]))
   #s((init:expr init 1) @ expr)])

(define-quoter syntax-quote-id @
  [(id:var ([name values]))
   #s((id:var id 1) @ name)]
  [(id:label ([name values]))
   #s((id:label id 1) @ name)]
  )

;(define syntax-quote-expr* (wrap syntax-quote-expr))
;(define syntax-quote-id* (wrap syntax-quote-id))
