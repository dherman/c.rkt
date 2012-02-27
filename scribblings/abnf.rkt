#lang at-exp racket/base

(require racket/match
         racket/list
         scribble/manual
         scribble/struct
         scribble/decode)
(require (for-syntax racket/base))

(provide BNF BNF-seq BNF-alt BNF-alt/close BNF-etc nonterm
         term BNF-var
         ABNF attr-decl attr-sel node-var attr-label
         optional BNF-group kleenestar kleeneplus kleenerange)

(define spacer (element 'hspace (list " ")))
(define equals (element 'tt (list spacer "::=" spacer)))
(define set (element #f (list (element 'tt (list spacer)) 'larr (element 'tt (list spacer)))))
(define alt (element 'tt (list spacer spacer "|" spacer spacer)))
(define attr (element 'tt (list spacer spacer spacer spacer spacer)))

(define (as-flow i) (make-flow (list (paragraph (list i)))))

(define (interleave l spacer)
  (element #f (cons (car l)
                    (apply append
                           (map (lambda (i)
                                  (list spacer i))
                                (cdr l))))))

(define (column lhs op rhs)
  (list (as-flow spacer) (as-flow lhs) (as-flow op) (as-flow rhs)))

(define (BNF . defns)
  (table
   #f
   (apply
    append
    (for/list ([defn defns])
      (match defn
        [(list A p1 ps ...)
         (cons (column A equals p1)
               (for/list ([p ps])
                 (column " " alt p)))])))))

(define (ABNF . defns)
  (table
   #f
   (apply
    append
    (for/list ([defn defns])
      (match defn
        [(list A (list p1 as1 ...) aps ...)
         (cons (column A equals p1)
               (append
                (for/list ([a1 as1])
                  (column " " attr a1))
                (append-map (match-lambda
                              [(list p as ...)
                               (cons (column " " alt p)
                                     (for/list ([a as])
                                       (column " " attr a)))])
                            aps)))])))))

(define-for-syntax (node-var? x)
  (let ([x (syntax->datum x)])
    (or (integer? x) (symbol? x))))

(define-syntax (node-var stx)
  (syntax-case stx ()
    [(_ name)
     (integer? (syntax->datum #'name))
     (with-syntax ([var (datum->syntax #'name (string->symbol (format "$~a" (syntax->datum #'name))))])
       #'(node-var var))]
    [(_ name)
     (identifier? #'name)
     (with-syntax ([var-name (symbol->string (syntax->datum #'name))])
       #'(schemeidfont var-name))]))

(define-syntax (attr-label stx)
  (syntax-case stx ()
    [(_ attribute)
     (identifier? #'attribute)
     (with-syntax ([attribute-name (symbol->string (syntax->datum #'attribute))])
       #'(element 'sf (list attribute-name)))]))

(define-syntax (attr-sel stx)
  (syntax-case stx ()
    [(_ name attribute)
     (and (node-var? #'name) (identifier? #'attribute))
     #'(element #f (append (list (node-var name))
                                (list (schemekeywordfont "."))
                                (list (attr-label attribute))))]))

(define-syntax (attr-decl stx)
  (syntax-case stx ()
    [(_ name attribute expr)
     (and (node-var? #'name) (identifier? #'attribute))
     #'(element #f (append (list (element 'tt (list spacer spacer spacer)))
                                (list (attr-sel name attribute))
                                (list set)
                                (list expr)))]))

(define (BNF-seq . l)
  (if (null? l)
      ""
      (interleave l spacer)))

(define (BNF-alt . l)
  (interleave l alt))

(define (BNF-alt/close . l)
  (interleave l " | "))

(define BNF-etc "...")

(define (BNF-var . s)
  (element 'italic (decode-content s)))

(define (nonterm #:sub [arg #f] . s)
  (if arg
      (element #f (append (list 'lang)
                          (list (element 'italic (decode-content s)))
                          (list 'rang)
                          (list (element 'subscript (list arg)))))
      (element #f (append (list 'lang)
                          (list (element 'italic (decode-content s)))
                          (list 'rang)))))

(define (term s)
  (schemevalfont (format "~v" s)))

(define (optional . s)
  (element #f (append (list "[") (decode-content s) (list "]"))))

(define (BNF-group . s)
  (element #f (append (list "{") 
                      (list (apply BNF-seq (decode-content s)))
                      (list "}"))))

(define (kleenestar . s)
  (element #f (append (decode-content s) (list "*"))))

(define (kleeneplus . s)
  (element #f (append (decode-content s) (list (element 'superscript (list "+"))))))

(define (kleenerange a b . s)
  (element #f (append (decode-content s) 
                      (list (element 'superscript 
                                     (list (format "{~a,~a}" a b)))))))
