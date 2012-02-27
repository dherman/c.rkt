#lang at-exp racket/base

(require (prefix-in set: (planet dherman/set:3/set))
         (planet dherman/io:1:9/io)
         racket/system
         racket/file
         racket/list
         racket/match
         "ast.rkt"
         "parse.rkt"
         "eval.rkt")
(require (for-syntax racket/base)
         (for-syntax "private/parser.rkt"))

(provide (struct-out abi))
(provide layout?
         ref-layout? struct-layout? union-layout? enum-layout? array-layout? pointer-layout? primitive-layout?
         layout-size layout-offset
         struct-layout-lookup union-layout-lookup deref-layout)
(provide make-header header? (rename-out [build-header header]))
(provide abi-lookup abi-lookup-typedef abi-lookup-tag serialize-abi deserialize-abi write-abi read-abi)

;; I think these are too unsafe to export:
;; (provide abi-add-typedef abi-add-tag)

(provide compile-header system-compiler)
(provide (struct-out query:sizeof)
         (struct-out query:offset)
         (struct-out query:expr)
         query?)

;; =============================================================================
;; DATA TYPE DEFINITIONS
;; =============================================================================

(define-struct internal:layout (size) #:prefab)
(define-struct (internal:layout:ref internal:layout) (name) #:prefab)
(define-struct (internal:layout:struct internal:layout) (tag fields) #:prefab)
(define-struct (internal:layout:union internal:layout) (tag variants) #:prefab)
(define-struct (internal:layout:enum internal:layout) (tag variants) #:prefab)
(define-struct (internal:layout:array internal:layout) (type length) #:prefab)
(define-struct (internal:layout:pointer internal:layout) (type) #:prefab)
(define-struct (internal:layout:primitive internal:layout) (name) #:prefab)

(define-struct layout (abi internal)
  #:transparent
  #:property prop:custom-write
  (lambda (wrapped port write?)
    (let ([internal (layout-internal wrapped)])
      (display
       (cond
         [(internal:layout:ref? internal) "#<ref-layout>"]
         [(internal:layout:struct? internal) "#<struct-layout>"]
         [(internal:layout:union? internal) "#<union-layout>"]
         [(internal:layout:enum? internal) "#<enum-layout>"]
         [(internal:layout:array? internal) "#<array-layout>"]
         [(internal:layout:pointer? internal) "#<pointer-layout>"]
         [(internal:layout:primitive? internal) "#<primitive-layout>"]
         [else "#<layout>"])
       port))))

;; abi * (union x (layout x)) -> (layout x)
(define (wrap abi x)
  (layout abi (soft-unwrap x)))

;; (layout x) -> x
(define (unwrap x)
  (layout-internal x))

;; (union x (layout x)) -> x
(define (soft-unwrap x)
  (if (layout? x) (unwrap x) x))

;; (layout internal:layout) -> boolean
(define struct-layout? (compose internal:layout:struct? unwrap))
(define union-layout? (compose internal:layout:union? unwrap))
(define enum-layout? (compose internal:layout:enum? unwrap))
(define array-layout? (compose internal:layout:array? unwrap))
(define pointer-layout? (compose internal:layout:pointer? unwrap))
(define ref-layout? (compose internal:layout:ref? unwrap))
(define primitive-layout? (compose internal:layout:primitive? unwrap))

;; TODO: cycle detection

;; layout -> layout
(define (deref-layout l)
  (match l
    [(struct layout (abi (struct internal:layout:ref (_ name))))
     (deref-layout (abi-lookup abi name))]
    [_ l]))

(define-struct query:sizeof (type) #:transparent)
(define-struct query:offset (type field) #:transparent)
(define-struct query:expr (expr) #:transparent)
(define (query? x)
  (or (query:sizeof? x)
      (query:offset? x)
      (query:expr? x)))

(define-struct header (headers))

;; exported as `header'
(define build-header
  (procedure-rename (lambda headers
                      (header headers))
                    'header))

(define-struct abi (typedefs tags)
  #:property prop:procedure
  (lambda (this key)
    (abi-lookup this key)))

;; =============================================================================
;; ABI OPERATIONS
;; =============================================================================

;; TODO: do a dotted-path lookup

;; abi * symbol -> layout
(define (abi-lookup abi key)
  (wrap abi
        (hash-ref (abi-typedefs abi)
                  key
                  (lambda ()
                    (hash-ref (abi-tags abi)
                              key
                              (lambda ()
                                (error 'abi-lookup "no definition for ~a" key)))))))

;; abi * symbol -> layout
(define (abi-lookup-typedef abi key)
  (wrap abi
        (hash-ref (abi-typedefs abi)
                  key
                  (lambda ()
                    (error 'abi-lookup-typedef "no definition for ~a" key)))))

;; abi * symbol -> layout
(define (abi-lookup-tag abi key)
  (wrap abi
        (hash-ref (abi-tags abi)
                  key
                  (lambda ()
                    (error 'abi-lookup-tag "no definition for ~a" key)))))

;; abi * symbol * (union internal:layout layout) -> abi
(define (abi-add-typedef a name type)
  (struct-copy abi a [typedefs (hash-set (abi-typedefs a) name (soft-unwrap type))]))

;; abi * symbol * (union internal:layout layout) -> abi
(define (abi-add-tag a name type)
  (struct-copy abi a [tags (hash-set (abi-tags a) name (soft-unwrap type))]))

;; sexp -> abi
(define (deserialize-abi sexp)
  (match sexp
    [`(abi ,typedefs ,tags)
     (abi typedefs tags)]
    [_ (raise-syntax-error 'read-abi "invalid serialized ABI" sexp)]))

;; abi -> sexp
(define (serialize-abi a)
  (match a
    [(struct abi (typedefs tags))
     `(abi ,typedefs ,tags)]))

;; abi [* output-port] -> void
(define (write-abi abi [out (current-output-port)])
  (write (serialize-abi abi) out))

;; [input-port] -> abi
(define (read-abi [in (current-input-port)])
  (deserialize-abi (read in)))

;; =============================================================================
;; LAYOUT OPERATIONS
;; =============================================================================

;; layout -> uint
(define layout-size
  (procedure-rename (compose internal:layout-size unwrap) 'layout-size))

(define (split-javadot sym)
  (map string->symbol (regexp-split #rx"\\." (symbol->string sym))))

;; layout * (union symbol (listof symbol)) -> uint
(define (layout-offset layout path)
  (cond
    [(symbol? path)
     (layout-offset layout (split-javadot path))]
    [(null? path) 0]
    [(struct-layout? layout)
     (match-let ([(list _ offset layout*) (struct-layout-lookup (car path) layout)])
       (+ offset (layout-offset layout* (cdr path))))]
    [(union-layout? layout)
     (match-let ([(list _ layout*) (union-layout-lookup (car path) layout)])
       (layout-offset layout* (cdr path)))]
    [(ref-layout? layout)
     (layout-offset (deref-layout layout) path)]
    ;; XXX: enums
    [else (error 'layout-offset "not a layout: ~a" layout)]))

;; symbol * struct-layout -> layout
(define (struct-layout-lookup field-name layout)
  (cond
    [(assq field-name (internal:layout:struct-fields (unwrap layout)))
     => (match-lambda
          [(list name offset layout*)
           (list name offset (wrap (layout-abi layout) layout*))])]
    [else (error 'struct-layout-lookup "field ~a not found" field-name)]))

;; symbol * union-layout -> layout
(define (union-layout-lookup variant-name layout)
  (cond
    [(assq variant-name (internal:layout:union-variants (unwrap layout)))
     => (match-lambda
          [(list name layout*)
           (list name (wrap (layout-abi layout) layout*))])]
    [else (error 'union-layout-lookup "variant ~a not found" variant-name)]))

;; =============================================================================
;; COMPILATION MONAD
;; =============================================================================

;; (compilation x) = (listof uint) abi -> (values (listof uint) abi x)

;; x -> (compilation x)
(define (return x)
  (lambda (compiled abi)
    (values compiled abi x)))

;; (compilation x) * (x -> (compilation y)) -> (compilation y)
(define (bind c f)
  (lambda (compiled1 abi1)
    (let-values ([(compiled2 abi2 x) (c compiled1 abi1)])
      ((f x) compiled2 abi2))))

;; [exact-positive-integer] -> (compilation uint)
(define (pop [n 1])
  (lambda (compiled abi)
    (if (< (length compiled) n)
        (error 'compile-header "unexpected end of compilation data")
        (let-values ([(head tail) (split-at compiled n)])
          (values tail abi head)))))
;    (if (null? compiled)
;        (error 'compile-header "unexpected end of compilation data")
;        (values (cdr compiled) abi (car compiled)))))

;; symbol * internal:layout -> (compilation #f)
(define (save-typedef name type)
  (lambda (compiled abi)
    (values compiled (abi-add-typedef abi name type) #f)))

;; symbol * internal:layout -> (compilation #f)
(define (save-tag tag type)
  (lambda (compiled abi)
    (values compiled (abi-add-tag abi tag type) #f)))

(define (for-each/m f . lss)
  (if (null? lss)
      (return #f)
      (for-each/m* f lss)))

(define (for-each/m* f lss)
  (if (ormap null? lss)
      (return #f)
      (let ([xs (map car lss)]
            [lss (map cdr lss)])
        (bind (apply f xs)
              (lambda (_)
                (for-each/m* f lss))))))

(define-syntax do
  (syntax-rules (<-)
    [(do s) s]
    [(do (x <- e) s ...)
     (bind e (lambda (x)
               (do s ...)))]
    [(do s1 s ...)
     (bind s1 (lambda (_) (do s ...)))]))

;; header * compiler -> abi
(define (compile-header header compiler)
  (let-values ([(queries computation deps) (precompile-header header)])
    ;; XXX: extend the computation to query the dependencies too
    (let-values ([(queries-left abi _) (computation (compiler queries) (abi #hasheq() #hasheq()))])
      abi)))

;; =============================================================================
;; PRECOMPILATION MONAD
;; =============================================================================

;; (precompilation x y) = x -> (values (listof query) (compilation y) (setof symbol))

;; (precompilation header #f)
(define (precompile-header header)
  (if (header? header)
      (precompile-compound-header header)
      (precompile-decl header)))

;; (precompilation compound-header #f)
(define (precompile-compound-header ch)
  (precompile-map precompile-header (header-headers ch)))

;; (precompilation decl #f)
(define (precompile-decl decl)
  (match decl
    [(struct decl:typedef (_ base declarators))
     (match (apply-declarator-contexts declarators base)
       [(list (struct decl:declarator (_ (struct id:var (_ ids)) types _)) ...)
        (let-values ([(queries compile-types deps) (precompile-map precompile-type types)])
          (values queries
                  (do
                    (types <- compile-types)
                    (for-each/m save-typedef ids types))
                  deps))])]
    [(struct decl:vars (_ _ base '()))
     (precompile-type base)]
    [(struct decl:vars (_ _ base declarators))
     (match (apply-declarator-contexts declarators base)
       [(list (struct decl:declarator (_ _ types _)) ...)
        (let-values ([(queries compile-types deps) (precompile-map precompile-type types)])
          (values queries
                  (do
                    compile-types
                    (return #f))
                  deps))])]
    #;[(struct decl:typedef (_ base (list (struct decl:declarator (_ id #f #f)))))
     (let-values ([(queries compile-type deps) (precompile-type base)])
       (values (cons (query:sizeof (id:var-name id)) queries)
               (do
                 (size <- (pop))
                 (type <- compile-type)
                 (save-typedef (id:var-name id) type))
               deps))]
#|
    [(struct decl:type:tagged ((struct type:struct (tag fields))))
     (let-values ([(queries compile-fields deps) (precompile-map (precompile-struct-field tag) fields)])
       (values (cons (query:sizeof `(struct ,tag)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (save-tag tag (internal:layout:struct size tag fields)))
               deps))]
    [(struct decl:type:tagged ((struct type:union (tag fields))))
     (let-values ([(queries compile-fields deps) (precompile-map precompile-union-field fields)])
       (values (cons (query:sizeof `(union ,tag)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (save-tag tag (internal:layout:union size tag fields)))
               deps))]
    #;[(struct decl:type:tagged ((struct type:enum ())))
     #f]
|#
    ))

(define (precompile-optional-type type)
  (if type (precompile-type type) (values null (return #f) set:empty)))

(define (precompile-type type)
  (match type
    [(struct type:qualified (_ type _))
     (precompile-type type)]
    [(struct type:primitive (_ name))
     (values (list (query:sizeof name))
             (do
               (size <- (pop))
               (return (internal:layout:primitive size name)))
             set:empty)]
    [(struct type:ref (_ (struct id:var (_ name))))
     (values (list (query:sizeof name))
             (do
               (size <- (pop))
               (return (internal:layout:ref size name)))
             (set:add name set:empty))]
    [(struct type:struct (_ #f fields))
     (let-values ([(queries compile-fields deps) (precompile-map (precompile-struct-field #f) fields)])
       (values (cons (query:sizeof `(struct ,@fields)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (return (internal:layout:struct size #f (apply append fields))))
               deps))]
    [(struct type:struct (_ (struct id:label (_ tag)) #f))
     (values (list (query:sizeof `(struct ,tag)))
             (do
               (size <- (pop))
               (return (internal:layout:struct size tag #f)))
             set:empty)]
    [(struct type:struct (_ (struct id:label (_ tag)) fields))
     (let-values ([(queries compile-fields deps) (precompile-map (precompile-struct-field tag) fields)])
       (values (cons (query:sizeof `(struct ,tag)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (fields* <- (return (apply append fields)))
                 (save-tag tag (internal:layout:struct size tag fields*))
                 (return (internal:layout:struct size tag fields*)))
               deps))]
    [(struct type:union (_ (struct id:label (_ tag)) #f))
     (values (list (query:sizeof `(struct ,tag)))
             (do
               (size <- (pop))
               (return (internal:layout:union size tag #f)))
             set:empty)]
    [(struct type:union (_ #f fields))
     (let-values ([(queries compile-fields deps) (precompile-map precompile-union-field fields)])
       (values (cons (query:sizeof `(struct ,@fields)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (return (internal:layout:union size #f (apply append fields))))
               deps))]
    [(struct type:union (_ (struct id:label (_ tag)) fields))
     (let-values ([(queries compile-fields deps) (precompile-map precompile-union-field fields)])
       (values (cons (query:sizeof `(union ,tag)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (fields* <- (return (apply append fields)))
                 (save-tag tag (internal:layout:union size tag fields*))
                 (return (internal:layout:union size tag fields*)))
               deps))]
    [(struct type:array (_ type _ _ length _))
     (let-values ([(queries compile-type deps) (precompile-type type)])
       (values (cons (query:sizeof `(array ,type ,length))
                     (cons (query:expr length)
                           queries))
               (do
                 (size <- (pop))
                 (length <- (pop))
                 (type <- compile-type)
                 (return (internal:layout:array size type length)))
               deps))]
    [(struct type:pointer (_ type _))
     (let-values ([(queries compile-type deps) (precompile-type type)])
       (values (cons (query:sizeof 'pointer) queries)
               (do
                 (size <- (pop))
                 (type <- compile-type)
                 (return (internal:layout:pointer type)))
               deps))]
#|
    [(struct type:union (#f fields))
     (let-values ([(queries compile-fields deps) (precompile-map precompile-union-field fields)])
       (values (cons (query:sizeof `(union ,@fields)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (return (internal:layout:union size #f fields)))
               deps))]
    [(struct type:union (tag #f))
     (values (list (query:sizeof `(union ,tag)))
             (do
               (size <- (pop))
               (return (internal:layout:union size tag #f)))
             set:empty)]
    [(struct type:union (tag fields))
     (let-values ([(queries compile-fields deps) (precompile-map precompile-union-field fields)])
       (values (cons (query:sizeof `(union ,tag)) queries)
               (do
                 (size <- (pop))
                 (fields <- compile-fields)
                 (save-tag tag (internal:layout:union size tag fields))
                 (return (internal:layout:union size tag fields)))
               deps))]
    [(struct type:array (type length))
     (let-values ([(queries compile-type deps) (precompile-type type)])
       (values (cons (query:sizeof `(array ,type ,length))
                     (cons (query:expr length)
                           queries))
               (do
                 (size <- (pop))
                 (length <- (pop))
                 (type <- compile-type)
                 (return (internal:layout:array size type length)))
               deps))]
    [(struct type:pointer (type))
     #f]
    [(struct type:function (return args))
     #f]
|#
    ))

;; symbol -> (precompilation (cons symbol type) (list symbol uint layout))
(define ((precompile-struct-field tag) field)
  (match field
    [(struct decl:member (_ base decls))
     (match (apply-member-declarator-contexts decls base)
       [(list (struct decl:member-declarator (_ (struct id:label (_ ids)) types _ #f)) ...)
        (let ([len (length decls)])
          (let-values ([(queries compile-types deps) (precompile-map precompile-type types)])
            (values (append (map (lambda (id) (query:offset `(struct ,tag) id)) ids) queries)
                    (do
                      (offsets <- (pop len))
                      (types <- compile-types)
                      (return (map list ids offsets types)))
                    deps)))])]
#|
       ;; XXX: generalize to multiple declarators
       [(list (struct decl:declarator (_ (struct id:var (_ name)) type _)))
        (let-values ([(queries compile-type deps) (precompile-type type)])
          (values (cons (query:offset `(struct ,tag) name) queries)
                  (do
                    (offset <- (pop))
                    (type <- compile-type)
                    (return (list name offset type)))
                  deps))])]
|#
#|
    [(cons name type)
     (let-values ([(queries compile-type deps) (precompile-optional-type type)])
       (values (cons (query:offset `(struct ,tag) name) queries)
               (do
                 (offset <- (pop))
                 (type <- compile-type)
                 (return (list name offset type)))
               deps))]
|#
    ))

;; (precompilation (cons symbol type) (list symbol layout))
(define (precompile-union-field field)
  (match field
    [(struct decl:member (_ base decls))
     (match (apply-member-declarator-contexts decls base)
       [(list (struct decl:member-declarator (_ (struct id:label (_ ids)) types _ #f)) ...)
        (let ([len (length decls)])
          (let-values ([(queries compile-types deps) (precompile-map precompile-type types)])
            (values queries
                    (do
                      (types <- compile-types)
                      (return (map list ids types)))
                    deps)))])]
#|
       ;; XXX: generalize to multiple declarators
       [(list (struct decl:declarator (_ (struct id:var (_ name)) type _)))
        (let-values ([(queries compile-type deps) (precompile-type type)])
          (values queries
                  (do
                    (type <- compile-type)
                    (return (list name type)))
                  deps))])]
|#
#|
    [(cons name type)
     (let-values ([(queries compile-type deps) (precompile-type type)])
       (values queries
               (do
                 (type <- compile-type)
                 (return (list name type)))
               deps))]
|#
    ))

;; (precompilation x y) * (listof x) -> (precompilation (listof x) (listof y))
(define (precompile-map f ls)
  (if (null? ls)
      (values null (return null) set:empty)
      (let-values ([(queries1 c1 deps1) (f (car ls))]
                   [(queries2 c2 deps2) (precompile-map f (cdr ls))])
        (values (append queries1 queries2)
                (bind c1 (lambda (x)
                           (bind c2 (lambda (y)
                                      (return (cons x y))))))
                (set:union deps1 deps2)))))

;; =============================================================================
;; SYSTEM HEADER COMPILER
;; =============================================================================

;; query -> void
(define (print-query query)
  (match query
    [(struct query:sizeof ((? symbol? type)))
     (printf "printf(\"%d ; (sizeof ~a)\\n\", sizeof(~a));\n" type type)]
    [(struct query:sizeof (`(struct ,tag)))
     (printf "printf(\"%d ; (sizeof (struct ~a))\\n\", sizeof(struct ~a));\n" tag tag)]
    [(struct query:sizeof ('pointer))
     (printf "printf(\"%d ; (sizeof pointer)\\n\", sizeof(void*));\n")]
    [(struct query:sizeof (type))
     (printf "printf(\"#f ; (sizeof ~a) -- *SKIPPED*\\n\");\n" type)]
    [(struct query:offset (`(struct ,tag) field-name))
     (printf "GET_OFFSET(struct ~a, ~a, off);\n" tag field-name)
     (printf "printf(\"%d ; (offset (struct ~a) ~a)\\n\", off);\n" tag field-name)]
    [(struct query:offset ((? symbol? type) field-name))
     (printf "GET_OFFSET(~a, ~a, off); \n" type field-name)
     (printf "printf(\"%d ; (offset ~a ~a)\\n\", off);\n" type field-name)]
    [(struct query:offset (type field-name))
     (printf "printf(\"#f ; (offset ~a ~a) -- *SKIPPED*\\n\");\n" type field-name)]
    #;[(struct query:expr (expr))
     (printf "printf(\"%d ; ~a\\n\", ~a);\n" expr (expression->C expr))]))

;; (listof query) [* (listof string)] -> void
(define (print-queries queries [includes null])
  (printf "#include <stdio.h>\n")
  (for ([include includes])
    (printf "#include ~a\n" include))
  
  (printf "#define GET_OFFSET(TYP, field, x) { TYP ___tmp___; x = (((int)(&___tmp___.field)) - ((int)(&___tmp___))); }\n")
  
  (printf "int main() {\n")
  (printf "int off;\n")
  
  (printf "printf(\"(\\n\");\n")
  (for-each print-query queries)
  (printf "printf(\")\\n\");\n")
  
  (printf "return 0;\n")
  (printf "}\n"))

#;(define (expression->C expr)
  (match expr
    [(? symbol?) (symbol->string expr)]
    [(struct expr:binop (op left right))
     (format "((~a) ~a (~a))" (expression->C left) op (expression->C right))]
    ;; TODO: this is grody and broken for chars, strings
    [(? expr:lit?)
     (format "~a" (expr:lit-value expr))]))

(define (system-compiler #:include<> [include<> null] #:include [include null] [exe gcc])
  (lambda (queries)
    (let ([headers (append (for/list ([i include<>])
                             (format "<~a>" i))
                           (for/list ([i include])
                             (format "\"~a\"" i)))])
      (let-values ([(stdout stderr) (exe (lambda ()
                                           (print-queries queries headers)))])
        (let ([result (read stdout)])
          result)))))
