#lang racket/base

(require "ast.rkt")
(require (for-syntax racket/base))

(provide typedef struct union enum array pointer)

(define-for-syntax (symbolic-identifier=? id1 id2)
  (eq? (syntax->datum id1) (syntax->datum id2)))

(define-syntax (@type stx)
  (syntax-case* stx (void char short int long float double signed unsigned _Bool _Complex) symbolic-identifier=?
    [(@type void)     #'(type:primitive #f 'void)]
    [(@type char)     #'(type:primitive #f 'char)]
    [(@type short)    #'(type:primitive #f 'short)]
    [(@type int)      #'(type:primitive #f 'int)]
    [(@type long)     #'(type:primitive #f 'long)]
    [(@type float)    #'(type:primitive #f 'float)]
    [(@type double)   #'(type:primitive #f 'double)]
    [(@type signed)   #'(type:primitive #f 'signed)]
    [(@type unsigned) #'(type:primitive #f 'unsigned)]
    [(@type _Bool)    #'(type:primitive #f '_Bool)]
    [(@type _Complex) #'(type:primitive #f '_Complex)]
    [(@type id)
     (identifier? #'id)
     #'(type:ref #f 'id)]
    [(@type t)
     #'(let ([tmp t])
         (if (decl:vars? tmp)
             (decl:vars-type tmp)
             tmp))]))

(define-syntax (typedef stx)
  (syntax-case stx ()
    [(_ t name)
     #'(decl:typedef #f (@type t) (list (decl:declarator #f (id:var #f 'name) #f #f)))]));'((name . #f)))]))

(define-syntax (struct stx)
  (syntax-case stx ()
    [(_ tag)
     (identifier? #'tag)
     #'(decl:vars #f #f (type:struct #f (id:label #f 'tag) #f) '())]
;     #'(decl:type:tagged (type:struct 'tag #f))]
    [(_ tag (field ...))
     (identifier? #'tag)
;     #'(decl:type:tagged (type:struct 'tag (list (struct-field field) ...)))]
     #'(decl:vars #f #f (type:struct #f (id:label #f 'tag) (list (struct-field field) ...)) '())]
    [(_ (field ...))
     #'(decl:vars #f #f (type:struct #f #f (list (struct-field field) ...)) '())]))
;     #'(decl:type:tagged (type:struct #f (list (struct-field field) ...)))]))

(define-syntax (struct-field stx)
  (syntax-case stx ()
    [(_ [t name])
     #'(decl:member #f (@type t) (list (decl:member-declarator #f (id:label #f 'name) #f #f #f)))]
;     #'`(name . ,(@type t))]
    [(_ name)
     ;; XXX: what is this silly case?
     #'(decl:member #f #f (list (decl:member-declarator #f (id:label #f 'name) #f #f)))]))
;     #'`(name . #f)]))

(define-syntax (union stx)
  (syntax-case stx ()
    [(_ tag)
     (identifier? #'tag)
     #'(decl:vars #f #f (type:union #f (id:label #f 'tag) #f) '())]
;     #'(decl:type:tagged (type:union 'tag #f))]
    [(_ tag (variant ...))
     (identifier? #'tag)
     #'(decl:vars #f #f (type:union #f (id:label #f 'tag) #f) (list (union-variant variant) ...))]
;     #'(decl:type:tagged (type:union 'tag (list (union-variant variant) ...)))]
    [(_ (variant ...))
     #'(decl:vars #f #f (type:union #f #f (list (union-variant variant) ...)))]))
;     #'(decl:type:tagged (type:union #f (list (union-variant variant) ...)))]))

(define-syntax (union-variant stx)
  (syntax-case stx ()
    [(_ [t name])
     #'(decl:member #f (@type t) (list (decl:member-declarator #f (@type t) #f #f)))]
;     #'`(name . ,(@type t))]
    [(_ name)
     ;; XXX: what is this silly case?
     #'(decl:member #f #f (list (decl:member-declarator #f (id:label #f 'name) #f #f)))]))
;     #'`(name . #f)]))

(define-syntax (enum stx)
  (syntax-case stx ()
    [(_ tag)
     (identifier? #'tag)
     #'(decl:type:tagged (type:enum 'tag #f))]
    [(_ tag (variant ...))
     (identifier? #'tag)
     #'(decl:type:tagged (type:enum 'tag (list (enum-variant variant) ...)))]
    [(_ (variant ...))
     #'(decl:type:tagged (type:enum #f (list (enum-variant variant) ...)))]))

(define-syntax (enum-variant stx)
  (syntax-case stx ()
    [(_ [name expr])
     #'`(name . ,(expr:lit 'int expr))]
    [(_ name)
     #'`(name . #f)]))

(define array (procedure-rename type:array 'array))
(define pointer (procedure-rename type:pointer 'pointer))
