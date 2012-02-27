#lang at-exp racket/base

(require (for-syntax racket/base)
         (for-syntax planet/util)
         scribble/manual
         scribble/eval
         scribble/basic
         (only-in srfi/13/string string-pad)
         racket/string
         racket/runtime-path
         "abnf.rkt")

(provide (all-defined-out))

(define-runtime-path home (build-path 'up))

;; HACK 1: this hijacks `system-compiler' to avoid actually calling GCC
;; HACK 2: it also hijacks `include/reader' just to change the directory
(define the-eval
  (let ([the-eval (make-base-eval)])
    (parameterize ([current-directory home])
      (the-eval `(require (prefix-in include: scheme/include)
                          (for-syntax scheme/base)
                          (for-syntax (file ,(path->string (build-path home "parse.ss"))))))
      (the-eval '(define-syntax (include/reader stx)
                   (syntax-case stx ()
                     [(_ fn expr)
                      (with-syntax ([fn* (datum->syntax #'fn
                                                        (string-append "scribblings/" (syntax->datum #'fn))
                                                        #'fn)])
                        #'(include:include/reader fn* expr))])))
      (the-eval `(require (file ,(path->string (build-path home "main.ss")))))
      (the-eval '(define (system-compiler #:include<> [include<> null] #:include [include null] exe)
                   (lambda (queries)
                     '(0 0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0)))))
    the-eval))

(define (bugref num [text (format "issue #~a" num)])
  (link (format "http://planet.plt-scheme.org/trac/ticket/~a" num) text))

(define (hist-item version year month day . text)
  (define (pad x n)
    (string-pad (format "~a" x) n #\0))
  (apply item (bold (format "Version ~a" version)) (format " (~a-~a-~a) - " year (pad month 2) (pad day 2)) text))

(define (version . parts)
  (string-join (for/list ([part parts]) (format "~a" part)) "."))

#|
(define-for-syntax (build-planet-string suffix)
  (format "~a/~a:~a:~a~a"
          (this-package-version-owner)
          (regexp-replace "\\.plt$" (this-package-version-name) "")
          (this-package-version-maj)
          (this-package-version-min)
          suffix))

(define-for-syntax (build-planet-id stx id)
  (datum->syntax
   stx
   (string->symbol
    (build-planet-string
     (if id (format "/~a" (syntax-e id)) "")))))

(define-syntax (this-package-version-id stx)
  (syntax-case stx ()
    [(tpvi) (build-planet-id stx #f)]
    [(tpvi name)
     (identifier? #'name)
     (build-planet-id stx #'name)]))

(define-syntax (scheme/this-package stx)
  (syntax-case stx ()
    [(sp)
     (quasisyntax/loc stx
       (scheme (planet #,(build-planet-id stx #f))))]
    [(sp name)
     (identifier? #'name)
     (quasisyntax/loc stx
       (scheme (planet #,(build-planet-id stx #'name))))]))

(define-syntax (defmodule/this-package stx)
  (syntax-case stx ()
    [(dmp)
     (quasisyntax/loc stx
       (defmodule (planet #,(build-planet-id stx #f))))]
    [(dmp name)
     (identifier? #'name)
     (quasisyntax/loc stx
       (defmodule (planet #,(build-planet-id stx #'name))))]))

(define-syntax (lang/this-package stx)
  (syntax-case stx ()
    [(sp)
     (quasisyntax/loc stx
       (SCHEME (UNSYNTAX (hash-lang)) planet #,(build-planet-id stx #f)))]
    [(sp name)
     (identifier? #'name)
     (quasisyntax/loc stx
       (SCHEME (UNSYNTAX (hash-lang)) planet #,(build-planet-id stx #'name)))]))
|#

(define id-or-tn
  @nonterm{AnyIdentifier})
;  @BNF-group[@nonterm{Identifier} "|" @nonterm{TypedefName}])

(define id-only
  @nonterm{Identifier})
