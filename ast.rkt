#lang racket/base

(require racket/match
         racket/path
         parser-tools/lex)
(require (for-syntax racket/base))

(provide (except-out (all-defined-out) stx-for-original-property))

(define-struct src (start-offset start-line start-col end-offset end-line end-col path) #:prefab)

(define (src-start src)
  (position (src-start-offset src)
            (src-start-line src)
            (src-start-col src)))

(define (src-end src)
  (position (src-end-offset src)
            (src-end-line src)
            (src-end-col src)))

(define (build-src start end path)
  (src (position-offset start)
       (position-line start)
       (position-col start)
       (position-offset end)
       (position-line end)
       (position-col end)
       path))

;; This syntax object will have the syntax-original? property. It can be used
;; with datum->syntax to give subsequent syntax objects this property.
(define stx-for-original-property (read-syntax #f (open-input-string "original")))

;; region->syntax : src [boolean] -> syntax
(define (src->syntax src [datum '...] [original? #t])
  (datum->syntax #f
                 datum
                 (list (src-path src)
                       (src-start-line src)
                       (src-start-col src)
                       (src-start-offset src)
                       (- (src-end-offset src) (src-start-offset src)))
                 (and original? stx-for-original-property)))

;; position position ... -> position
(define (position-max pos . poss)
  (cond
    [(null? poss) pos]
    [(> (position-offset pos) (position-offset (car poss)))
     (apply position-max pos (cdr poss))]
    [else (apply position-max poss)]))

;; position position ... -> position
(define (position-min pos . poss)
  (cond
    [(null? poss) pos]
    [(< (position-offset pos) (position-offset (car poss)))
     (apply position-min pos (cdr poss))]
    [else (apply position-min poss)]))

;; src src ... -> src
(define (src-range src . srcs)
  (src (apply min (src-start-offset src) (map src-start-offset srcs))
       (apply min (src-start-line src) (map src-start-line srcs))
       (apply min (src-start-col src) (map src-start-col srcs))
       (apply max (src-end-offset src) (map src-end-offset srcs))
       (apply max (src-end-line src) (map src-end-line srcs))
       (apply max (src-end-col src) (map src-end-col srcs))
       (src-path src)))

(define (primitive-type-specifier? x)
  (and (memq x '(void char short int long float double signed unsigned _Bool _Complex)) #t))

(define (unary-operator? x)
  (and (memq x '(& * + - ~ !)) #t))

(define (binary-operator? x)
  (and (memq x '(* / % + - << >> < > <= >= == !== & ^ \| && \|\|)) #t))

(define (assignment-operator? x)
  (and (memq x '(= *= /= %= += -= <<= >>= &= ^= \|=)) #t))

(define (increment-operator? x)
  (and (memq x '(++ --)) #t))

(define (apply-type-context ctxt x)
  (cond
    [(not ctxt) x]
    [(type:pointer? ctxt)
     (struct-copy type:pointer ctxt [base (apply-type-context (type:pointer-base ctxt) x)])]
    [(type:array? ctxt)
     (struct-copy type:array ctxt [base (apply-type-context (type:array-base ctxt) x)])]
    [(type:function? ctxt)
     (struct-copy type:function ctxt [return (apply-type-context (type:function-return ctxt) x)])]
    [else (error 'parse "cannot plug into unexpected context: ~a" ctxt)]))

;; type * (listof (decl:declarator type-context)) -> (values (listof type) (listof decl:declarator))
;(define (build-declarators base declarators)
;  (for/fold ([types null]
;             [declarators null])
;            ([declarator declarators])
;    (match declarator
;      [(struct decl:declarator (src #f context init))
;       (values (cons (apply-type-context context base) types) declarators)]
;      [(struct decl:declarator (src id context init))
;       (values types (decl:declarator src id (apply-type-context context base) init))])))

;; declarator-context * type -> complete-declarator
(define (apply-declarator-context declarator base)
  (match declarator
    [(struct decl:declarator (src id context init))
     (decl:declarator src id (apply-type-context context base) init)]))

;; (listof declarator-context) * type -> (listof complete-declarator)
(define (apply-declarator-contexts declarators base)
  (map (lambda (declarator)
         (apply-declarator-context declarator base))
       declarators))

;; member-declarator-context * type -> complete-member-declarator
(define (apply-member-declarator-context declarator base)
  (match declarator
    [(struct decl:member-declarator (src id context init bit-size))
     (decl:member-declarator src id (apply-type-context context base) init bit-size)]))

;; (listof member-declarator-context) * type -> (listof complete-member-declarator)
(define (apply-member-declarator-contexts declarators base)
  (map (lambda (declarator)
         (apply-member-declarator-context declarator base))
       declarators))


;; =============================================================================
;; EXPRESSIONS
;; =============================================================================

(define-struct expr (src) #:prefab)

(define-struct (expr:ref            expr) (id) #:prefab)
(define-struct (expr:int            expr) (value qualifiers) #:prefab)
(define-struct (expr:float          expr) (value qualifiers) #:prefab)
(define-struct (expr:char           expr) (source wide?) #:prefab)
(define-struct (expr:string         expr) (source wide?) #:prefab)
(define-struct (expr:compound       expr) (type inits) #:prefab)
(define-struct (expr:array-ref      expr) (expr offset) #:prefab)
(define-struct (expr:call           expr) (function arguments) #:prefab)
(define-struct (expr:member         expr) (expr label) #:prefab)
(define-struct (expr:pointer-member expr) (expr label) #:prefab)
(define-struct (expr:postfix        expr) (expr op) #:prefab)
(define-struct (expr:prefix         expr) (op expr) #:prefab)
(define-struct (expr:cast           expr) (type expr) #:prefab)
(define-struct (expr:sizeof         expr) (term) #:prefab)
(define-struct (expr:unop           expr) (op expr) #:prefab)
(define-struct (expr:binop          expr) (left op right) #:prefab)
(define-struct (expr:assign         expr) (left op right) #:prefab)
(define-struct (expr:begin          expr) (left right) #:prefab)
(define-struct (expr:if             expr) (test cons alt) #:prefab)


;; =============================================================================
;; STATEMENTS
;; =============================================================================

(define-struct stmt (src) #:prefab)

(define-struct (stmt:label    stmt) (label stmt) #:prefab)
(define-struct (stmt:case     stmt) (expr stmt) #:prefab)
(define-struct (stmt:default  stmt) (stmt) #:prefab)
(define-struct (stmt:block    stmt) (items) #:prefab)
(define-struct (stmt:expr     stmt) (expr) #:prefab)
(define-struct (stmt:if       stmt) (test cons alt) #:prefab)
(define-struct (stmt:switch   stmt) (test body) #:prefab)
(define-struct (stmt:while    stmt) (test body) #:prefab)
(define-struct (stmt:do       stmt) (body test) #:prefab)
(define-struct (stmt:for      stmt) (init test update body) #:prefab)
(define-struct (stmt:goto     stmt) (label) #:prefab)
(define-struct (stmt:continue stmt) () #:prefab)
(define-struct (stmt:break    stmt) () #:prefab)
(define-struct (stmt:return   stmt) (result) #:prefab)
(define-struct (stmt:empty    stmt) () #:prefab)


;; =============================================================================
;; DECLARATIONS
;; =============================================================================

(define-struct decl (src) #:prefab)

(define-struct (decl:typedef           decl) (type declarators) #:prefab)
(define-struct (decl:vars              decl) (storage-class type declarators) #:prefab)
(define-struct (decl:formal            decl) (storage-class type declarator) #:prefab)
(define-struct (decl:function          decl) (storage-class inline? return-type declarator preamble body) #:prefab)
(define-struct (decl:declarator        decl) (id type initializer) #:prefab)
(define-struct (decl:member            decl) (type declarators) #:prefab)
(define-struct (decl:member-declarator decl) (id type initializer bit-size) #:prefab)

(define (declarator-context? x)
  (and (decl:declarator? x)
       (type-context? (decl:declarator-type x))))

(define (complete-declarator? x)
  (and (decl:declarator? x)
       (complete-type? (decl:declarator-type x))))

(define (member-declarator-context? x)
  (and (decl:member-declarator? x)
       (type-context? (decl:member-declarator-type x))))

(define (complete-member-declarator? x)
  (and (decl:member-declarator? x)
       (complete-type? (decl:member-declarator-type x))))


;; =============================================================================
;; INITIALIZERS
;; =============================================================================

(define-struct init (src) #:prefab)

(define-struct (init:compound init) (elements) #:prefab)
(define-struct (init:expr     init) (expr) #:prefab)


;; =============================================================================
;; DESIGNATORS
;; =============================================================================

(define-struct dtor (src) #:prefab)

(define-struct (dtor:array  dtor) (expr) #:prefab)
(define-struct (dtor:member dtor) (label) #:prefab)


;; =============================================================================
;; TYPES
;; =============================================================================

(define-struct type (src) #:prefab)

(define-struct (type:primitive type) (name) #:prefab)
(define-struct (type:ref       type) (id) #:prefab)
(define-struct (type:struct    type) (tag fields) #:prefab)
(define-struct (type:union     type) (tag variants) #:prefab)
(define-struct (type:enum      type) (tag variants) #:prefab)
(define-struct (type:array     type) (base static? qualifiers length star?) #:prefab)
(define-struct (type:pointer   type) (base qualifiers) #:prefab)
(define-struct (type:function  type) (return formals) #:prefab)
(define-struct (type:qualified type) (type qualifiers) #:prefab)
  
(define (type-context? x)
  (or (not x)
      (and (type:pointer? x) (type-context? (type:pointer-base x)))
      (and (type:array? x) (type-context? (type:array-base x)))
      (and (type:function? x) (type-context? (type:function-return x)))))

(define (complete-type? x)
  (and (type? x)
       (not (type-context? x))))


;; =============================================================================
;; IDENTIFIERS
;; =============================================================================

(define-struct id (src) #:prefab)

(define-struct (id:var       id) (name) #:prefab)
(define-struct (id:label     id) (name) #:prefab)
(define-struct (id:qualifier id) (name) #:prefab)
(define-struct (id:op        id) (name) #:prefab)
(define-struct (id:ellipsis  id) () #:prefab)
(define-struct (id:storage   id) (class) #:prefab)
(define-struct (id:inline    id) () #:prefab)
(define-struct (id:star      id) () #:prefab)

(define (id-name id)
  (match id
    [(or (struct id:var (_ name))
         (struct id:label (_ name))
         (struct id:qualifier (_ name))
         (struct id:op (_ name))
         (struct id:storage (_ name)))
     name]
    [(? id:ellipsis?) '...]
    [(? id:inline?) 'inline]
    [(? id:star?) '*]))

(define (id->syntax id [original? #t])
  (match id
    [(or (struct id:var (src sym))
         (struct id:label (src sym))
         (struct id:storage (src sym))
         (struct id:qualifier (src sym)))
     (src->syntax src sym original?)]
    [(struct id:inline (src))
     (src->syntax src 'inline original?)]
    [(struct id:ellipsis (src))
     (src->syntax src '... original?)]
    [(struct id:star (src))
     (src->syntax src '* original?)]))
