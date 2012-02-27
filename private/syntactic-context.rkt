#lang racket/base

(require racket/match
         parser-tools/lex
         (except-in "../ast.rkt" declarator-context? type-context?))

(provide (all-defined-out))

;; NOTES
;; -----
;; - we don't need to track struct tags, since they cause no parsing ambiguities
;; - caching id's only needed for typedef;
;;   o enumerators can go into effect immediately
;;   o ... what else?

;; SCOPE BLOCKS
;; ------------
;; - compound statement
;; - function parameter list
;; - function declarator-list/body
;; - for-declaration

;; COMMA
;; -----
;; scoping:
;;  - init-declarator list
;;  - enumerator list
;;  - parameter list
;;  - identifier list
;; non-scoping:
;;  - initializer list
;;  - argument expression list
;;  - expression sequence
;;  - member-declarator list

;; SEMI-COLON
;; ----------
;; scoping:
;;  - global declaration
;;  - for-head declaration
;; non-scoping:
;;  - function preamble
;;  - struct declaration
;;  - expression statement
;;  - do-while statement
;;  - for-head expression
;;  - jump statement

;; EQUAL
;; -----
;; scoping:
;;  - init-declarator
;; non-scoping
;;  - initializer list

(define-syntax-rule (debug sexp-expr)
  (log-debug (format "~v" sexp-expr)))

(define-syntax-rule (warning sexp-expr)
  (log-warning (format "~v" sexp-expr)))

;; =============================================================================
;; LEXER STATE
;; =============================================================================

(define-struct lexer-state
  (read?                           ; boolean
   source                          ; any
   offset                          ; (or position #f)
   [declarators #:mutable]         ; (listof (box (or #f symbol)))
   [brace-depth #:mutable]         ; nat
   [parenthesis-depth #:mutable]   ; nat
   [previous-token #:mutable]))    ; (or #f token)

(define (new-lexer-state declarators [read? #f] [source #f] [offset #f])
  (lexer-state read? source offset declarators 0 0 #f))

(define (parenthesis++ state)
  (set-lexer-state-parenthesis-depth! state (add1 (lexer-state-parenthesis-depth state))))

(define (parenthesis-- state)
  (set-lexer-state-parenthesis-depth! state (sub1 (lexer-state-parenthesis-depth state))))

(define (brace++ state)
  (set-lexer-state-brace-depth! state (add1 (lexer-state-brace-depth state))))

(define (brace-- state)
  (set-lexer-state-brace-depth! state (sub1 (lexer-state-brace-depth state))))

(define (save-token! state x)
  (set-lexer-state-previous-token! state (if (position-token? x) (position-token-token x) x)))

;; =============================================================================
;; PARSER STATE
;; =============================================================================

;; major ::= 'block | 'formals | 'preamble | 'struct | 'union | 'enum | 'statement
;; minor ::= #f | 'typedef | 'for

(define-struct parser-state
  ([env #:mutable]                 ; (listof (listof (cons symbol (or 'var 'type))))
   [declarators #:mutable]         ; (listof (box (or #f symbol)))
   [major-context #:mutable]       ; (listof major)
   [minor-context #:mutable]))     ; (listof minor)
   
(define (new-parser-state declarators initial-env initial-context)
  (parser-state (list initial-env) declarators (list initial-context) (list #f)))

(define (push-context! state major)
  (set-parser-state-major-context! state (cons major (parser-state-major-context state)))
  (set-parser-state-minor-context! state (cons #f (parser-state-minor-context state))))

(define (pop-context! state)
  (set-parser-state-major-context! state (cdr (parser-state-major-context state)))
  (set-parser-state-minor-context! state (cdr (parser-state-minor-context state))))

(define (set-minor-context! state minor)
  (set-parser-state-minor-context! state (cons minor (cdr (parser-state-minor-context state)))))

(define (cache-declarator-id! ps id)
  (set-box! (car (parser-state-declarators ps)) id))

(define (push-scope! state)
  (set-parser-state-env! state (cons null (parser-state-env state))))

(define (pop-scope! state)
  (set-parser-state-env! state (cdr (parser-state-env state))))

(define (add-binding! name kind state)
  (match (parser-state-env state)
    [(cons rib env)
     (set-parser-state-env! state (cons (cons (cons name kind) rib) env))]))

(define (lookup x e)
  (cond
    [(null? e) #f]
    [(assq x (car e)) => cdr]
    [else (lookup x (cdr e))]))

(define (declarator-context? ps)
  (and (memq (car (parser-state-major-context ps)) '(block formals preamble)) #t))

(define (typedef-context? ps)
  (eq? (car (parser-state-minor-context ps)) 'typedef))

;; =============================================================================
;; JOINT OPERATIONS
;; =============================================================================

(define (new-state [read? #f] [source #f] [offset #f] [initial-env null] [initial-context 'block])
  (let ([declarators (list (box #f))])
    (values (new-parser-state declarators initial-env initial-context)
            (new-lexer-state declarators read? source offset))))

(define (looking-ahead? ls ps)
  (cond
    [(eq? (car (lexer-state-declarators ls))
          (car (parser-state-declarators ps)))
     #t]
    [(eq? (car (lexer-state-declarators ls))
          (car (cdr (parser-state-declarators ps))))
     #f]
    [else
     (warning '(C state "lexer and parser out of sync"))
     (warning (marshall-state ps ls))
     #t]))

(define (looked-ahead? ls ps)
  (cond
    [(eq? (car (lexer-state-declarators ls))
          (car (parser-state-declarators ps)))
     #f]
    [(eq? (car (lexer-state-declarators ls))
          (car (cdr (parser-state-declarators ps))))
     #t]
    [else
     (warning '(C state "lexer and parser out of sync"))
     (warning (marshall-state ps ls))
     #f]))

(define (push-declarator! ps ls)
  (let ([declarator (box #f)])
    (set-lexer-state-declarators! ls (cons declarator (lexer-state-declarators ls)))
    (set-parser-state-declarators! ps (cons declarator (parser-state-declarators ps)))))

(define (pop-declarator! ps ls)
  (cond
    [(unbox (car (parser-state-declarators ps)))
     => (lambda (id)
          (debug `(C context pop-declarator ,id))
          (add-binding! id (if (typedef-context? ps) 'type 'var) ps))]
    [else (debug '(C context pop-declarator #f))])
  (set-lexer-state-declarators! ls (cdr (lexer-state-declarators ls)))
  (set-parser-state-declarators! ps (cdr (parser-state-declarators ps)))
  (debug (marshall-state ps ls)))

(define (pop-parser-declarator! ps ls)
  (cond
    [(and (declarator-context? ps)
          (not (looked-ahead? ls ps))
          (unbox (car (parser-state-declarators ps))))
     => (lambda (id)
          (debug `(C context pop-parser-declarator ,id))
          (add-binding! id (if (typedef-context? ps) 'type 'var) ps))]
    [else (debug `(C context pop-parser-declarator #f))])
  (set-parser-state-declarators! ps (cdr (parser-state-declarators ps)))
  (debug (marshall-state ps ls)))

(define (pop-lexer-declarator! ls ps token)
  (when (looking-ahead? ls ps)
    (cond
      [(unbox (car (lexer-state-declarators ls)))
       => (lambda (id)
            (debug `(C context pop-lexer-declarator (lookahead ,token) (popped ,id)))
            (add-binding! id (if (typedef-context? ps) 'type 'var) ps))]
      [(memq (token-name (lexer-state-previous-token ls)) '(IDENTIFIER TYPEDEF_NAME))
       (debug `(C context pop-lexer-declarator (lookahead ,token) (previous ,(lexer-state-previous-token ls))))
       (add-binding! (token-value (lexer-state-previous-token ls))
                     (if (typedef-context? ps) 'type 'var)
                     ps)]
      [else
       (debug `(C context pop-lexer-declarator (lookahead ,token) (popped #f) (previous #f)))]))
  (set-lexer-state-declarators! ls (cdr (lexer-state-declarators ls))))

(define (marshall-state ps ls)
  `(C context
      (lexer ,(lexer-state-declarators ls)
             ,(lexer-state-brace-depth ls)
             ,(lexer-state-parenthesis-depth ls)
             ,(lexer-state-previous-token ls))
      (parser ,(parser-state-declarators ps)
              ,(parser-state-major-context ps)
              ,(parser-state-minor-context ps)
              ,(parser-state-env ps))))

;(define (dump-context parser-state lexer-state [out (current-error-port)])
;  (fprintf out "+++ LEXING CONTEXT  +++\n")
;  (fprintf out "decl depth: ~a, brace depth: ~a, paren depth: ~a\n"
;           (length (lexer-state-declarators lexer-state))
;           (lexer-state-brace-depth lexer-state)
;           (lexer-state-parenthesis-depth lexer-state))
;  (fprintf out "previous token: ~a\n" (lexer-state-previous-token lexer-state))
;  (fprintf out "declarators: ~a\n" (lexer-state-declarators lexer-state))
;  (fprintf out "+++ PARSING CONTEXT +++\n")
;  (fprintf out "decl depth: ~a, major: ~a, minor: ~a\n"
;           (length (parser-state-declarators parser-state))
;           (parser-state-major-context parser-state)
;           (parser-state-minor-context parser-state))
;  (fprintf out "declarators: ~a\n" (parser-state-declarators parser-state))
;  (fprintf out "environment:\n")
;  (for ([frame (parser-state-env parser-state)])
;    (fprintf out "[ ")
;    (for ([cell frame])
;      (display cell out)
;      (display " " out))
;    (fprintf out " ]\n"))
;  (fprintf out "+++++++++++++++++++++++\n"))

#|
(define (cache-declarator-id! id ps ls)
  (if (and (declaration-context? ps)
           (= (parser-state-declaration-depth ps) (lexer-state-declaration-depth ls)))
      (begin
        (printf "caching id ~a~n" id)
        (cache-identifier! ps id))
        ;(set-parser-state-cached-identifier! ps id))
      (printf "not caching id ~a~n" id)))

;; XXX: rename this, it's not dequeueing
(define (dequeue-declaration! parser-state lexer-state)
  (unless (declaration-context? parser-state)
    (printf "not a declaration context~n"))
  (when (declaration-context? parser-state)
    (cond
      [(cached-identifier parser-state) ;(parser-state-cached-identifier parser-state)
       => (lambda (id)
            (printf "dequeue-declaration! --> adding cached ~a~n" id)
            (add-binding! id (if (typedef-context? parser-state) 'type 'var) parser-state))]
      ;; XXX: can we prove this will always be there if there hasn't been a queued name?
      [else
       (printf "dequeue-declaration! --> adding previous token ~a~n" (token-value (lexer-state-previous-token lexer-state)))
       (add-binding! (token-value (lexer-state-previous-token lexer-state))
                     (if (typedef-context? parser-state) 'type 'var)
                     parser-state)]))
  ;(set-parser-state-cached-identifier! parser-state #f)
  )
|#
