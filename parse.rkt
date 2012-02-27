#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     parser-tools/lex)
         (for-syntax "private/parser.rkt")
         (for-syntax "ast.rkt"))
(require racket/include
         "private/parser.rkt"
         "ast.rkt")

(provide parse-program parse-declaration parse-statement parse-expression parse-type-expression)
(provide program declaration statement expression type-expression)
(provide program-reader)
#;(provide include-c)

;; =============================================================================
;; COMPILE-TIME PARSING
;; =============================================================================

(define-for-syntax (string-literal-value stx)
  (let ([datum (syntax->datum stx)])
    (cond
      [(syntax-property stx 'scribble)
       => (lambda (p)
            (cond
              [(eq? p 'indentation) ""]
              [(and (pair? p) (eq? (car p) 'newline)) (cadr p)]
              [else datum]))]
      [else datum])))

(define-for-syntax (syntax-list x)
  (if (syntax? x) (syntax->list x) x))

(define-for-syntax (identifier-list? x)
  (andmap identifier? (syntax-list x)))

(define-for-syntax (string-list? x)
  (andmap (compose string? syntax->datum) (syntax-list x)))

(define-for-syntax (parse-scribble-strings name parser ts ss)
  (let* ([src (apply string-append (map string-literal-value ss))]
         [typedefs (map syntax->datum ts)]
         [src1 (car ss)]
         [offset (position (syntax-position src1)
                           (syntax-line src1)
                           (syntax-column src1))])
    (parameterize ([current-syntax-error-target name])
      (parser src #:typedef typedefs #:source (syntax-source src1) #:offset offset))))

(define-syntax (declaration stx)
  (syntax-case stx ()
    [(_ #:typedef (t ...) strings ...)
     (and (identifier-list? #'(t ...))
          (string-list? #'(strings ...)))
     (with-syntax ([ast (parse-scribble-strings 'declaration
                                                parse-declaration
                                                (syntax->list #'(t ...))
                                                (syntax->list #'(strings ...)))])
       #'(quote ast))]
    [(_ strings ...)
     (string-list? #'(strings ...))
     (syntax/loc stx
       (declaration #:typedef () strings ...))]))

(define-syntax (program stx)
  (syntax-case stx ()
    [(_ #:typedef (t ...) strings ...)
     (and (identifier-list? #'(t ...))
          (string-list? #'(strings ...)))
     (with-syntax ([ast (parse-scribble-strings 'program
                                                parse-program
                                                (syntax->list #'(t ...))
                                                (syntax->list #'(strings ...)))])
       #'(quote ast))]
    [(_ strings ...)
     (string-list? #'(strings ...))
     (syntax/loc stx
       (program #:typedef () strings ...))]))

(define-syntax (statement stx)
  (syntax-case stx ()
    [(_ #:typedef (t ...) strings ...)
     (and (identifier-list? #'(t ...))
          (string-list? #'(strings ...)))
     (with-syntax ([ast (parse-scribble-strings 'statement
                                                parse-statement
                                                (syntax->list #'(t ...))
                                                (syntax->list #'(strings ...)))])
       #'(quote ast))]
    [(_ strings ...)
     (string-list? #'(strings ...))
     (syntax/loc stx
       (statement #:typedef () strings ...))]))

(define-syntax (expression stx)
  (syntax-case stx ()
    [(_ #:typedef (t ...) strings ...)
     (and (identifier-list? #'(t ...))
          (string-list? #'(strings ...)))
     (with-syntax ([ast (parse-scribble-strings 'expression
                                                parse-expression
                                                (syntax->list #'(t ...))
                                                (syntax->list #'(strings ...)))])
       #'(quote ast))]
    [(_ strings ...)
     (string-list? #'(strings ...))
     (syntax/loc stx
       (expression #:typedef () strings ...))]))

(define-syntax (type-expression stx)
  (syntax-case stx ()
    [(_ #:typedef (t ...) strings ...)
     (and (identifier-list? #'(t ...))
          (string-list? #'(strings ...)))
     (with-syntax ([ast (parse-scribble-strings 'type-expression
                                                parse-type-expression
                                                (syntax->list #'(t ...))
                                                (syntax->list #'(strings ...)))])
       #'(quote ast))]
    [(_ strings ...)
     (string-list? #'(strings ...))
     (syntax/loc stx
       (type-expression #:typedef () strings ...))]))

(define (program-reader #:typedef [typedef null])
  (lambda (name in)
    (if (eof-object? (peek-char in))
        eof
        (with-syntax ([(x ...) (parse-program in #:source name #:typedef typedef)])
          #''(x ...)))))
