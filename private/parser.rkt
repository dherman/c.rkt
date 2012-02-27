#lang racket/base

(require (for-syntax racket/base))
(require parser-tools/lex
         parser-tools/yacc
         racket/match
         racket/list
         syntax/readerr
         (except-in "../ast.rkt" declarator-context? type-context?)
         "lexer.rkt"
         "syntactic-context.rkt")

(provide parse-program parse-declaration parse-statement parse-expression parse-type-expression
         current-syntax-error-target)

(define current-syntax-error-target (make-parameter 'parse))

(define syntax-error-includes-location? (make-parameter #f))

;; XXX: forbid typedefs in illegal contexts (to prevent strange environment entries)

;; 6.7.2 #2: Type specifier constraints
;; Note: all the valid sequences in the spec are in reverse alphabetical order.
(define (parse-primitive-type-specifier x)
  (let* ([sorted (sort x string>? #:key (compose symbol->string type:primitive-name))]
         [sorted-names (map type:primitive-name sorted)])
    (match sorted-names
      [(list (? symbol? t))
       (car sorted)]
      [(or '(signed char)
           '(unsigned char)
           '(signed short)
           '(short int)
           '(signed short int)
           '(unsigned short)
           '(unsigned short int)
           '(signed int)
           '(unsigned int)
           '(signed long)
           '(long int)
           '(signed long int)
           '(unsigned long)
           '(unsigned long int)
           '(long long)
           '(signed long long)
           '(long long int)
           '(signed long long int)
           '(unsigned long long)
           '(unsigned long long int)
           '(long double)
           '(float _Complex)
           '(double _Complex)
           '(long double _Complex))
       (type:primitive (apply src-range (map type-src sorted)) sorted-names)]
      [_ (raise-syntax-error (current-syntax-error-target) "invalid type specifiers" (src->syntax (apply src-range (map type-src x)) sorted-names))])))

(define (check-function-declarator! decl)
  (match decl
    [(struct decl:declarator (src id type _))
     (when (type:pointer? type)
       (raise-syntax-error (current-syntax-error-target) "function declared with pointer type" (src->syntax src (id:var-name id))))
     (when (type:array? type)
       (raise-syntax-error (current-syntax-error-target) "function declared with array type" (src->syntax src (id:var-name id))))
     (unless (type:function? type)
       (raise-syntax-error (current-syntax-error-target) "function not declared with a function type" (src->syntax src (id:var-name id))))
     (when (type:function? (type:function-return type))
       (raise-syntax-error (current-syntax-error-target) "function declared as returning a function" (src->syntax src (id:var-name id))))
     (when (type:array? (type:function-return type))
       (raise-syntax-error (current-syntax-error-target) "function declared as returning an array" (src->syntax src (id:var-name id))))]))

(define (add-function-bindings! decl ps)
  (match decl
    [(struct decl:declarator (_
                              (struct id:var (_ f))
                              (struct type:function (_ _ (list (struct decl:formal (_ _ _ (struct decl:declarator (_ (struct id:var (_ xs)) _ _)))) ...))) _))
     (add-binding! f 'var ps)
     (for ([x xs])
       (add-binding! x 'var ps))]
    [(struct decl:declarator (_ (struct id:var (_ f)) (struct type:function (_ _ (list formals ...))) _))
     (add-binding! f 'var ps)
     ;; XXX: what else could these params be?
     ]))

(define (parse-storage-class storage-class-specs)
  (when (> (length storage-class-specs) 1)
    (raise-syntax-error (current-syntax-error-target)
                        "multiple storage class specifiers"
                        (src->syntax (apply src-range (map id-src storage-class-specs))
                                     (map id:storage-class storage-class-specs))))
  (and (pair? storage-class-specs) (car storage-class-specs)))

(define (parse-type-specifiers specifiers)
  (cond
    [(null? specifiers) #f]
    [(andmap type:primitive? specifiers)
     (parse-primitive-type-specifier specifiers)]
    [(null? (cdr specifiers))
     (car specifiers)]
    ;; TODO: are there cases where this is ok? can it even come up?
    [else (raise-syntax-error (current-syntax-error-target) "too many type specifiers" (src->syntax (apply src-range (map type-src specifiers))))]))

(define (qualify type qualifiers)
  (if (null? qualifiers)
      type
      (let* ([qualifiers-src (map id-src qualifiers)]
             [src (apply src-range
                         (if type (cons (type-src type) qualifiers-src) qualifiers-src))])
        (type:qualified src type qualifiers))))

(define (parse-spec-quals spec-quals)
  (let-values ([(qualifiers type-specs) (partition id:qualifier? spec-quals)])
    (qualify (parse-type-specifiers type-specs) qualifiers)))

(define (parse-decl-specs decl-specs)
  (let*-values ([(storage-class-specs decl-specs) (partition id:storage? decl-specs)]
                [(function-specifiers decl-specs) (partition id:inline? decl-specs)]
                [(qualifiers type-specs) (partition id:qualifier? decl-specs)])
    (values (parse-storage-class storage-class-specs)
            (qualify (parse-type-specifiers type-specs) qualifiers)
            (and (pair? function-specifiers) (car function-specifiers)))))

(define (build-declaration decl-specs decls src)
  (let-values ([(storage-class type inline?) (parse-decl-specs decl-specs)])
    ;; XXX: put the inline somewhere
    (if (and storage-class (eq? (id:storage-class storage-class) 'typedef))
        (decl:typedef src type decls)
        (decl:vars src storage-class type decls))))

(define (build-member-declaration decl-specs decls src)
  (let-values ([(qualifiers type-specs) (partition id:qualifier? decl-specs)])
    (decl:member src (qualify (parse-type-specifiers type-specs) qualifiers) decls)))

(define (build-parameter-declaration decl-specs decl src)
  (let-values ([(storage-class type inline?) (parse-decl-specs decl-specs)])
    (when (and storage-class (eq? (id:storage-class storage-class) 'typedef))
      (raise-syntax-error (current-syntax-error-target) "typedef in formal parameters" (id->syntax storage-class)))
    (when inline?
      (raise-syntax-error (current-syntax-error-target) "inline formal parameter" (id->syntax inline?)))
    (unless (or (not storage-class) (eq? (id:storage-class storage-class) 'register))
      (raise-syntax-error (current-syntax-error-target) (format "illegal storage class for formal parameter ~a" (id:var-name (decl:declarator-id decl))) (id->syntax storage-class)))
    (decl:formal src storage-class type decl)))

(define (build-type-name spec-quals context)
  (apply-type-context context (parse-spec-quals spec-quals)))

(define (build-function-definition decl-specs decl preamble body src)
  (let-values ([(storage-class type inline?) (parse-decl-specs decl-specs)])
    (when (and storage-class (eq? (id:storage-class storage-class) 'typedef))
      (raise-syntax-error (current-syntax-error-target) "typedef as function definition" (id->syntax storage-class)))
    (decl:function src storage-class inline? type decl preamble body)))

(define (declarator->member-declarator decl)
  (match decl
    [(struct decl:declarator (src1 (struct id:var (src2 name)) type init))
     (decl:member-declarator src1 (id:label src2 name) type init #f)]))

(define (as-bit-field-declarator decl bit-size src-end)
  (match decl
    [(struct decl:member-declarator (src-start id type init _))
     (decl:member-declarator (src-range src-start src-end) id type init bit-size)]))

;; =============================================================================
;; PARSER
;; =============================================================================

(define (position->string p)
  (format "~a:~a" (position-line p) (position-col p)))

(define (src->string src)
  (format "~a - ~a" (position->string (src-start src)) (position->string (src-end src))))

(define (c-parser ps ls)
  (define build-offset-src
    (let ([source (lexer-state-source ls)])
      (cond
        [(lexer-state-offset ls)
         => (match-lambda
              [(struct position (base-offset base-line base-col))
               (let ([base-offset (sub1 base-offset)]
                     [base-line (sub1 base-line)])
                 (lambda (start-pos end-pos)
                   (let ([start-line (position-line start-pos)]
                         [end-line (position-line end-pos)])
                     (src (+ base-offset (position-offset start-pos))
                          (+ base-line start-line)
                          (if (= start-line 1)
                              (+ base-col (position-col start-pos))
                              (position-col start-pos))
                          (+ base-offset (position-offset end-pos))
                          (+ base-line end-line)
                          (if (= end-line 1)
                              (+ base-col (position-col end-pos))
                              (position-col end-pos))
                          source))))])]
        [else (lambda (start-pos end-pos)
                (build-src start-pos end-pos source))])))
  (define-syntax (@ stx)
    (syntax-case stx ()
      [(_ end)
       #'(@ 1 end)]
      [(_ start end)
       (with-syntax ([start-pos (datum->syntax
                                 #'end
                                 (string->symbol
                                  (format "$~a-start-pos" (syntax->datum #'start))))]
                     [end-pos (datum->syntax
                               #'end
                               (string->symbol
                                (format "$~a-end-pos" (syntax->datum #'end))))])
         #'(build-offset-src start-pos end-pos))]))
  (parser
   (start TranslationUnit ExternalDeclaration Statement Expression TypeName)
   (end EOF)
   (src-pos)
   (tokens BasicTokens Keywords Operators Separators EmptyLiterals)
   #;(debug "debug.yacc.txt")
   (error (lambda (token-ok? token-name token-value start-pos end-pos)
            (cond
              [(and token-ok? (eq? token-name 'NUMBER_ERROR))
               (raise-read-error (format "bad number literal: ~a" token-value)
                                 (lexer-state-source ls)
                                 (position-line start-pos)
                                 (position-col start-pos)
                                 (position-offset start-pos)
                                 (- (position-offset end-pos)
                                    (position-offset start-pos)))]
              [(and token-ok? (eq? token-name 'STRING_ERROR))
               (raise-read-error (format "bad string literal: ~v"
                                         (string-append "\"" (string-error-string token-value)))
                                 (lexer-state-source ls)
                                 (position-line start-pos)
                                 (position-col start-pos)
                                 (position-offset start-pos)
                                 (- (position-offset end-pos)
                                    (position-offset start-pos)))]
              [(and token-ok? (eq? token-name 'READ_ERROR))
               (raise-read-error (string-append "read: " token-value)
                                 (lexer-state-source ls)
                                 (position-line start-pos)
                                 (position-col start-pos)
                                 (position-offset start-pos)
                                 (- (position-offset end-pos)
                                    (position-offset start-pos)))]
              [token-ok?
               (let ([loc (build-offset-src start-pos end-pos)]
                     ;; XXX: use a short description instead of token-name?
                     [desc (or token-value token-name)])
                 (let ([msg (if (eq? token-name 'IDENTIFIER)
                                "unexpected identifier (perhaps missing a typedef declaration?)"
                                (string-append "unexpected " (token-description token-name)))])
                   (raise-syntax-error (current-syntax-error-target)
                                       (if (syntax-error-includes-location?)
                                           (format "~a [~a]" msg (src->string loc))
                                           msg)
                                       (id->syntax (id:label loc desc)))))]
              [else
               (raise-read-error (format "bad token")
                                 (lexer-state-source ls)
                                 (position-line start-pos)
                                 (position-col start-pos)
                                 (position-offset start-pos)
                                 (- (position-offset end-pos)
                                    (position-offset start-pos)))])))

   (grammar

    ;; EFFECTFUL PRODUCTIONS

    (!PushScope
     [() (push-scope! ps)])

    (!PopScope
     [() (pop-scope! ps)])

    (!PushDeclarator
     [()
      (begin (debug '(C parser push declarator))
             (push-declarator! ps ls)
             (debug (marshall-state ps ls)))])

    (!PopParserDeclarator
     [()
      (begin (debug '(C parser pop parser-declarator))
             (pop-parser-declarator! ps ls))])

    (!PopDeclarator
     [()
      (begin (debug '(C parser pop declarator))
             (pop-declarator! ps ls))])

    (!PushBlockContext
     [()
      (begin (debug '(C parser push block-context))
             (push-context! ps 'block))])

    (!PushFormalsContext
     [()
      (begin (debug '(C parser push formals-context))
             (push-context! ps 'formals))])

    (!PushPreambleContext
     [()
      (begin (debug '(C parser push preamble-context))
             (push-context! ps 'preamble))])

    (!PushStatementContext
     [()
      (begin (debug '(C parser push statement-context))
             (push-context! ps 'statement))])

    (!PushStructContext
     [()
      (begin (debug '(C parser push struct-context))
             (push-context! ps 'struct))])

    (!PushUnionContext
     [()
      (begin (debug '(C parser push union-context))
             (push-context! ps 'struct))])

    (!PushEnumContext
     [()
      (begin (debug '(C parser push enum-context))
             (push-context! ps 'enum))])

    (!PopContext
     [()
      (begin (debug '(C parser pop context))
             (pop-context! ps))])

    (!TypedefContext
     [()
      (begin (debug '(C parser update typedef-context))
             (set-minor-context! ps 'typedef))])

    (!ForContext
     [()
      (begin (debug '(C parser update for-context))
             (set-minor-context! ps 'for))])

    (!CacheDeclaratorIdentifier
     [(IDENTIFIER)
      (begin
        (when (declarator-context? ps)
          (debug `(C parser cache (declarator ,$1)))
          (cache-declarator-id! ps $1))
        (id:var (@ 1) $1))])

    (!CacheDeclaratorTypedefName
     [(TYPEDEF_NAME)
      (begin
        (when (declarator-context? ps)
          (debug `(C parser cache (declarator ,$1)))
          (cache-declarator-id! ps $1))
        (id:var (@ 1) $1))])

    ;; A.1.3 Identifiers

    (Identifier
     [(IDENTIFIER)
      (id:var (@ 1) $1)])

    (IdentifierLabel
     [(IDENTIFIER)
      (id:label (@ 1) $1)])

    (TypedefName
     [(TYPEDEF_NAME)
      (id:var (@ 1) $1)])

    (TypedefNameLabel
     [(TYPEDEF_NAME)
      (id:label (@ 1) $1)])

    ;; A.1.5 Constants

    (Constant
     [(IntegerConstant) $1]
     [(FloatingConstant) $1]
     [(CharacterConstant) $1])

    (IntegerConstant
     [(UNSIGNED_LONG_INTEGER_LIT)
      (expr:int (@ 1) $1 '(unsigned long))]
     [(LONG_INTEGER_LIT)
      (expr:int (@ 1) $1 '(long))]
     [(UNSIGNED_LONG_LONG_INTEGER_LIT)
      (expr:int (@ 1) $1 '(unsigned long long))]
     [(LONG_LONG_INTEGER_LIT)
      (expr:int (@ 1) $1 '(long long))]
     [(UNSIGNED_INTEGER_LIT)
      (expr:int (@ 1) $1 '(unsigned))]
     [(INTEGER_LIT)
      (expr:int (@ 1) $1 '())])

    (FloatingConstant
     [(FLOAT_LIT)
      (expr:float (@ 1) $1 '())]
     [(DOUBLE_LIT)
      (expr:float (@ 1) $1 '(double))]
     [(LONG_DOUBLE_LIT)
      (expr:float (@ 1) $1 '(long double))])

    (CharacterConstant
     [(CHAR_LIT)
      (expr:char (@ 1) $1 #f)]
     [(WCHAR_LIT)
      (expr:char (@ 1) $1 #t)])

    ;; A.1.6 String Literals

    (StringLiteral
     [(STRING_LIT)
      (expr:string (@ 1) $1 #f)]
     [(WSTRING_LIT)
      (expr:string (@ 1) $1 #t)])


    ;; A.2.1 Expressions

    (PrimaryExpression
     [(Identifier)
      (expr:ref (@ 1) $1)]
     [(Constant) $1]
     [(StringLiteral) $1]
     [(O_PAREN Expression C_PAREN) $2])

    (PostfixExpression
     [(PrimaryExpression) $1]
     [(PostfixExpression O_BRACKET Expression C_BRACKET)
      (expr:array-ref (@ 4) $1 $3)]
     [(PostfixExpression O_PAREN ArgumentExpressionList C_PAREN)
      (expr:call (@ 4) $1 $3)]
     [(PostfixExpression O_PAREN C_PAREN)
      (expr:call (@ 3) $1 '())]
     [(PostfixExpression PERIOD IdentifierLabel)
      (expr:member (@ 3) $1 $3)]
     [(PostfixExpression PERIOD TypedefNameLabel)
      (expr:member (@ 3) $1 $3)]
     [(PostfixExpression -> IdentifierLabel)
      (expr:pointer-member (@ 3) $1 $3)]
     [(PostfixExpression -> TypedefNameLabel)
      (expr:pointer-member (@ 3) $1 $3)]
     [(PostfixExpression ++)
      (expr:postfix (@ 2) $1 (id:op (@ 2 2) '++))]
     [(PostfixExpression --)
      (expr:postfix (@ 2) $1 (id:op (@ 2 2) '--))]
     [(O_PAREN TypeName C_PAREN O_BRACE InitializerList C_BRACE)
      (expr:compound (@ 6) $2 $5)]
     [(O_PAREN TypeName C_PAREN O_BRACE InitializerList COMMA C_BRACE)
      (expr:compound (@ 7) $2 $5)])

    (ArgumentExpressionList
     [(AssignmentExpression) (list $1)]
     [(ArgumentExpressionList COMMA AssignmentExpression) (append $1 (list $3))])

    (UnaryExpression
     [(PostfixExpression) $1]
     [(++ UnaryExpression)
      (expr:prefix (@ 2) (id:op (@ 1 1) '++) $2)]
     [(-- UnaryExpression)
      (expr:prefix (@ 2) (id:op (@ 1 1) '--) $2)]
     [(UnaryOperator CastExpression)
      (expr:unop (@ 2) $1 $2)]
     [(sizeof UnaryExpression)
      (expr:sizeof (@ 2) $2)]
     [(sizeof O_PAREN TypeName C_PAREN)
      (expr:sizeof (@ 4) $3)])

    (UnaryOperator
     [(&) (id:op (@ 1) '&)]
     [(*) (id:op (@ 1) '*)]
     [(+) (id:op (@ 1) '+)]
     [(-) (id:op (@ 1) '-)]
     [(~) (id:op (@ 1) '~)]
     [(!) (id:op (@ 1) '!)])

    (CastExpression
     [(UnaryExpression) $1]
     [(O_PAREN TypeName C_PAREN CastExpression)
      (expr:cast (@ 4) $2 $4)])

    (MultiplicativeExpression
     [(CastExpression) $1]
     [(MultiplicativeExpression * CastExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '*) $3)]
     [(MultiplicativeExpression / CastExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '/) $3)]
     [(MultiplicativeExpression % CastExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '%) $3)])

    (AdditiveExpression
     [(MultiplicativeExpression) $1]
     [(AdditiveExpression + MultiplicativeExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '+) $3)]
     [(AdditiveExpression - MultiplicativeExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '-) $3)])

    (ShiftExpression
     [(AdditiveExpression) $1]
     [(ShiftExpression << AdditiveExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '<<) $3)]
     [(ShiftExpression >> AdditiveExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '>>) $3)])

    (RelationalExpression
     [(ShiftExpression) $1]
     [(RelationalExpression < ShiftExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '<) $3)]
     [(RelationalExpression > ShiftExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '>) $3)]
     [(RelationalExpression <= ShiftExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '<=) $3)]
     [(RelationalExpression >= ShiftExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '>=) $3)])

    (EqualityExpression
     [(RelationalExpression) $1]
     [(EqualityExpression == RelationalExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '==) $3)]
     [(EqualityExpression != RelationalExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '!=) $3)])

    (ANDExpression
     [(EqualityExpression) $1]
     [(ANDExpression & EqualityExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '&) $3)])

    (ExclusiveORExpression
     [(ANDExpression) $1]
     [(ExclusiveORExpression ^ ANDExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '^) $3)])

    (InclusiveORExpression
     [(ExclusiveORExpression) $1]
     [(InclusiveORExpression PIPE ExclusiveORExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '\|) $3)])

    (LogicalANDExpression
     [(InclusiveORExpression) $1]
     [(LogicalANDExpression && InclusiveORExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '&&) $3)])

    (LogicalORExpression
     [(LogicalANDExpression) $1]
     [(LogicalORExpression OR LogicalANDExpression)
      (expr:binop (@ 3) $1 (id:op (@ 2 2) '\|\|) $3)])

    (ConditionalExpression
     [(LogicalORExpression) $1]
     [(LogicalORExpression ? Expression COLON ConditionalExpression)
      (expr:if (@ 5) $1 $3 $5)])

    (AssignmentExpression?
     [() #f]
     [(AssignmentExpression) $1])

    (AssignmentExpression
     [(ConditionalExpression) $1]
     [(UnaryExpression AssignmentOperator AssignmentExpression)
      (expr:assign (@ 3) $1 $2 $3)])

    (AssignmentOperator
     [(=) (id:op (@ 1) '=)]
     [(*=) (id:op (@ 1) '*=)]
     [(/=) (id:op (@ 1) '/=)]
     [(%=) (id:op (@ 1) '%=)]
     [(+=) (id:op (@ 1) '+=)]
     [(-=) (id:op (@ 1) '-=)]
     [(<<=) (id:op (@ 1) '<<=)]
     [(>>=) (id:op (@ 1) '>>=)]
     [(&=) (id:op (@ 1) '&=)]
     [(^=) (id:op (@ 1) '^=)]
     [(OREQUAL) (id:op (@ 1) '\|=)])

    (Expression
     [(AssignmentExpression) $1]
     [(Expression COMMA AssignmentExpression)
      (expr:begin (@ 3) $1 $3)])

    (Expression?
     [() #f]
     [(Expression) $1])

    (ConstantExpression
     [(ConditionalExpression) $1])


    ;; A.2.2 Declarations

    (Declaration
     [(DeclarationModifiers SEMI_COLON)
      (build-declaration $1 '() (@ 2))]
     [(DeclarationSpecifiers SEMI_COLON)
      (build-declaration $1 '() (@ 2))]
     [(DeclarationModifiers InitDeclaratorList-tn !PopParserDeclarator SEMI_COLON)
      (build-declaration $1 $2 (@ 4))]
     [(DeclarationSpecifiers InitDeclaratorList !PopParserDeclarator SEMI_COLON)
      (build-declaration $1 $2 (@ 4))])

    ;; These rules track the fact that no TypeSpecifiers have been provided.
    (DeclarationModifiers
     [(DeclarationModifier)
      (list $1)]
     [(DeclarationModifiers DeclarationModifier)
      (append $1 (list $2))])

    (DeclarationModifier
     [(StorageClassSpecifier) $1]
     [(TypeQualifier) $1]
     [(FunctionSpecifier) $1])

    ;; These rules track the fact that at least one TypeSpecifier has been provided.
    (DeclarationSpecifiers
     [(DeclarationSpecifiers+prim) $1]
     [(DeclarationSpecifiers+tagged) $1]
     [(DeclarationSpecifiers+tn) $1])

    ;; This makes sure there is at least one primitive-type-specifier.
    (DeclarationSpecifiers+prim
     [(prim+DeclarationSpecifiers+prim) $1]
     [(prim+DeclarationSpecifiers+prim DeclarationModifiers)
      (append $1 $2)]
     [(DeclarationModifiers prim+DeclarationSpecifiers+prim)
      (append $1 $2)]
     [(DeclarationModifiers prim+DeclarationSpecifiers+prim DeclarationModifiers)
      (cons $1 (append $2 $3))])

    (prim+DeclarationSpecifiers+prim
     [(PrimTypeSpecifier)
      (list $1)]
     [(prim+DeclarationSpecifiers+prim DeclarationModifiers PrimTypeSpecifier)
      (append $1 $2 (list $3))]
     [(prim+DeclarationSpecifiers+prim PrimTypeSpecifier)
      (append $1 (list $2))])

    ;; This makes sure there is exactly one tagged-type-specifier.
    (DeclarationSpecifiers+tagged
     [(TaggedTypeSpecifier)
      (list $1)]
     [(DeclarationSpecifiers+tagged DeclarationModifier)
      (append $1 (list $2))]
     [(DeclarationModifiers TaggedTypeSpecifier)
      (append $1 (list $2))])

    ;; This makes sure there is exactly one typedef-name.
    (DeclarationSpecifiers+tn
     [(TypedefName)
      (list (type:ref (@ 1) $1))]
     [(DeclarationSpecifiers+tn DeclarationModifier)
      (append $1 (list $2))]
     [(DeclarationModifiers TypedefName)
      (append $1 (list (type:ref (@ 2 2) $2)))])

    (InitDeclaratorList
     [(InitDeclarator)
      (list $1)]
     [(InitDeclaratorList !PopParserDeclarator COMMA InitDeclarator)
      (append $1 (list $4))])

    ;; (declarator symbol type-context (or initializer #f))
    (InitDeclarator
     [(!PushDeclarator Declarator) $2]
     [(!PushDeclarator Declarator !PopParserDeclarator = Initializer !PushDeclarator)
      (match-let ([(struct decl:declarator (_ id type _)) $2])
        (decl:declarator (@ 2 5) id type $5))])

    (InitDeclaratorList-tn
     [(InitDeclarator-tn)
      (list $1)]
     [(InitDeclaratorList-tn !PopParserDeclarator COMMA InitDeclarator-tn)
      (append $1 (list $4))])

    ;; (declarator symbol type-context (or initializer #f))
    (InitDeclarator-tn
     [(!PushDeclarator Declarator-tn) $2]
     [(!PushDeclarator Declarator-tn !PopParserDeclarator = Initializer !PushDeclarator)
      (match-let ([(struct decl:declarator (_ id type _)) $2])
        (decl:declarator (@ 2 5) id type $5))])

    ;; id:storage
    (StorageClassSpecifier
     [(!TypedefContext typedef) (id:storage (@ 2 2) 'typedef)]
     [(extern) (id:storage (@ 1) 'extern)]
     [(static) (id:storage (@ 1) 'static)]
     [(auto) (id:storage (@ 1) 'auto)]
     [(register) (id:storage (@ 1) 'register)])

    (TypeSpecifier
     [(PrimTypeSpecifier) $1]
     [(TaggedTypeSpecifier) $1]
     [(TypedefName) (type:ref (@ 1) $1)])

    (PrimTypeSpecifier
     [(void) (type:primitive (@ 1) 'void)]
     [(char) (type:primitive (@ 1) 'char)]
     [(short) (type:primitive (@ 1) 'short)]
     [(int) (type:primitive (@ 1) 'int)]
     [(long) (type:primitive (@ 1) 'long)]
     [(float) (type:primitive (@ 1) 'float)]
     [(double) (type:primitive (@ 1) 'double)]
     [(signed) (type:primitive (@ 1) 'signed)]
     [(unsigned) (type:primitive (@ 1) 'unsigned)]
     [(_Bool) (type:primitive (@ 1) '_Bool)]
     [(_Complex) (type:primitive (@ 1) '_Complex)])

    (TaggedTypeSpecifier
     [(struct IdentifierLabel !PushStructContext O_BRACE C_BRACE !PopContext)
      (type:struct (@ 6) $2 '())]
     [(struct TypedefNameLabel !PushStructContext O_BRACE C_BRACE !PopContext)
      (type:struct (@ 6) $2 '())]
     [(struct IdentifierLabel !PushStructContext O_BRACE StructDeclarationList C_BRACE !PopContext)
      (type:struct (@ 7) $2 $5)]
     [(struct TypedefNameLabel !PushStructContext O_BRACE StructDeclarationList C_BRACE !PopContext)
      (type:struct (@ 7) $2 $5)]
     [(struct !PushStructContext O_BRACE StructDeclarationList C_BRACE !PopContext)
      (type:struct (@ 6) #f $4)]
     [(struct IdentifierLabel)
      (type:struct (@ 2) $2 #f)]
     [(struct TypedefNameLabel)
      (type:struct (@ 2) $2 #f)]
     [(union IdentifierLabel !PushUnionContext O_BRACE C_BRACE !PopContext)
      (type:union (@ 6) $2 '())]
     [(union TypedefNameLabel !PushUnionContext O_BRACE C_BRACE !PopContext)
      (type:union (@ 6) $2 '())]
     [(union IdentifierLabel !PushUnionContext O_BRACE StructDeclarationList C_BRACE !PopContext)
      (type:union (@ 7) $2 $5)]
     [(union TypedefNameLabel !PushUnionContext O_BRACE StructDeclarationList C_BRACE !PopContext)
      (type:union (@ 7) $2 $5)]
     [(union !PushUnionContext O_BRACE StructDeclarationList C_BRACE !PopContext)
      (type:union (@ 6) #f $4)]
     [(union IdentifierLabel)
      (type:union (@ 2) $2 #f)]
     [(union TypedefNameLabel)
      (type:union (@ 2) $2 #f)]
     [(EnumSpecifier) $1])

    (StructDeclarationList
     [(StructDeclaration) (list $1)]
     [(StructDeclarationList StructDeclaration)
      (append $1 (list $2))])

    (StructDeclaration
     [(TypeQualifierList SEMI_COLON)
      (build-member-declaration $1 '() (@ 2))]
;      (decl:member (@ 2) $1 '())]
     [(StructSpecifiers SEMI_COLON)
      (build-member-declaration $1 '() (@ 2))]
;      (decl:member (@ 2) $1 '())]
     [(TypeQualifierList StructDeclaratorList-tn SEMI_COLON)
      (build-member-declaration $1 $2 (@ 3))]
;      (decl:member (@ 3) $1 $2)]
     [(StructSpecifiers StructDeclaratorList SEMI_COLON)
      (build-member-declaration $1 $2 (@ 3))])
;      (decl:member (@ 3) $1 $2)])

    (StructSpecifiers
     [(SpecifierQualifierList+prim) $1]
     [(SpecifierQualifierList+tagged) $1]
     [(SpecifierQualifierList+tn) $1])

    (SpecifierQualifierList+prim
     [(prim+SpecifierQualifierList+prim) $1]
     [(prim+SpecifierQualifierList+prim TypeQualifierList)
      (append $1 $2)]
     [(TypeQualifierList prim+SpecifierQualifierList+prim)
      (append $1 $2)]
     [(TypeQualifierList prim+SpecifierQualifierList+prim TypeQualifierList)
      (cons $1 (append $2 $3))])

    (prim+SpecifierQualifierList+prim
     [(PrimTypeSpecifier) (list $1)]
     [(prim+SpecifierQualifierList+prim TypeQualifierList PrimTypeSpecifier)
      (append $1 $2 (list $3))]
     [(prim+SpecifierQualifierList+prim PrimTypeSpecifier)
      (append $1 (list $2))])

    (SpecifierQualifierList+tagged
     [(TaggedTypeSpecifier) (list $1)]
     [(SpecifierQualifierList+tagged TypeQualifier)
      (append $1 (list $2))]
     [(TypeQualifierList TaggedTypeSpecifier)
      (append $1 (list $2))])

    (SpecifierQualifierList+tn
     [(TypedefName)
      (list (type:ref (@ 1) $1))]
     [(SpecifierQualifierList+tn TypeQualifier)
      (append $1 (list $2))]
     [(TypeQualifierList TypedefName)
      (append $1 (list (type:ref (@ 2 2) $2)))])

    (StructDeclaratorList-tn
     [(StructDeclarator-tn) $1]
     [(StructDeclaratorList-tn COMMA StructDeclarator-tn) (append $1 (list $3))])

    (StructDeclarator-tn
     [(Declarator-tn) (declarator->member-declarator $1)]
     [(Declarator-tn COLON ConstantExpression)
      (as-bit-field-declarator (declarator->member-declarator $1) $3 (@ 3 3))]
     [(COLON ConstantExpression)
      (decl:member-declarator (@ 2) #f #f #f $2)])

    (SpecifierQualifierList
     [(TypeSpecifier) (list $1)]
     [(TypeQualifier) (list $1)]
     [(SpecifierQualifierList TypeSpecifier) (append $1 (list $2))]
     [(SpecifierQualifierList TypeQualifier) (append $1 (list $2))])

    (StructDeclaratorList
     [(StructDeclarator) (list $1)]
     [(StructDeclaratorList COMMA StructDeclarator) (append $1 (list $3))])

    (StructDeclarator
     [(Declarator) (declarator->member-declarator $1)]
     [(Declarator COLON ConstantExpression)
      (as-bit-field-declarator (declarator->member-declarator $1) $3 (@ 3 3))]
     [(COLON ConstantExpression)
      (decl:member-declarator (@ 2) #f #f #f $2)])

    (EnumSpecifier
     [(enum IdentifierLabel !PushEnumContext O_BRACE EnumeratorList C_BRACE !PopContext)
      (type:enum (@ 7) $2 $5)]
     [(enum TypedefNameLabel !PushEnumContext O_BRACE EnumeratorList C_BRACE !PopContext)
      (type:enum (@ 7) $2 $5)]
     [(enum !PushEnumContext O_BRACE EnumeratorList C_BRACE !PopContext)
      (type:enum (@ 6) #f $4)]
     [(enum IdentifierLabel !PushEnumContext O_BRACE EnumeratorList COMMA C_BRACE !PopContext)
      (type:enum (@ 8) $2 $5)]
     [(enum TypedefNameLabel !PushEnumContext O_BRACE EnumeratorList COMMA C_BRACE !PopContext)
      (type:enum (@ 8) $2 $5)]
     [(enum !PushEnumContext O_BRACE EnumeratorList COMMA C_BRACE !PopContext)
      (type:enum (@ 7) #f $4)]
     [(enum IdentifierLabel)
      (type:enum (@ 2) $2 #f)]
     [(enum TypedefNameLabel)
      (type:enum (@ 2) $2 #f)])

    (EnumeratorList
     [(Enumerator)
      (list $1)]
     [(EnumeratorList COMMA Enumerator)
      (append $1 (list $3))])

    (Enumerator
     [(TYPEDEF_NAME)
      (begin (add-binding! $1 'var ps)
             (id:var (@ 1) $1))]
     [(IDENTIFIER)
      (begin (add-binding! $1 'var ps)
             (id:var (@ 1) $1))]
     [(TYPEDEF_NAME = ConstantExpression)
      (begin
        (add-binding! $1 'var ps)
        (cons (id:var (@ 1) $1) $3))]
     [(IDENTIFIER = ConstantExpression)
      (begin
        (add-binding! $1 'var ps)
        (cons (id:var (@ 1) $1) $3))])

    (TypeQualifier
     [(const) (id:qualifier (@ 1) 'const)]
     [(restrict) (id:qualifier (@ 1) 'restrict)]
     [(volatile) (id:qualifier (@ 1) 'volatile)])

    (FunctionSpecifier
     [(inline) (id:inline (@ 1))])

    ;; (declarator symbol type-context #f)
    (Declarator
     [(Pointer DirectDeclarator)
      (match-let ([(struct decl:declarator (_ id type init)) $2])
        (decl:declarator (@ 2) id ($1 type) init))]
;      ($1 $2)]
     [(DirectDeclarator) $1])

    ;; (declarator symbol type-context #f)
    (Declarator-tn
     [(Pointer DirectDeclarator-tn)
      (match-let ([(struct decl:declarator (_ id type init)) $2])
        (decl:declarator (@ 2) id ($1 type) init))]
;      ($1 $2)]
     [(DirectDeclarator-tn) $1])

    ;; (declarator symbol type-context #f)
    (Declarator-paren-tn
     [(Pointer DirectDeclarator-paren-tn)
      (match-let ([(struct decl:declarator (_ id type init)) $2])
        (decl:declarator (@ 2) id ($1 type) init))]
     [(DirectDeclarator-paren-tn) $1])

    ;; (declarator symbol type-context #f)
    (DirectDeclarator
     [(!CacheDeclaratorIdentifier)
      (decl:declarator (@ 1) $1 #f #f)]
     [(!CacheDeclaratorTypedefName)
      (decl:declarator (@ 1) $1 #f #f)]
     [(O_PAREN Declarator C_PAREN)
      $2]
     [(DirectDeclarator O_BRACKET TypeQualifierList? AssignmentExpression? C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 5) id (apply-type-context context (type:array (@ 2 5) #f #f $3 $4 #f)) #f))]
     [(DirectDeclarator O_BRACKET static TypeQualifierList? AssignmentExpression C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 6) id (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 3 3) 'static) $4 $5 #f)) #f))]
     [(DirectDeclarator O_BRACKET TypeQualifierList static AssignmentExpression C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 6) id (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 4 4) 'static) $3 $5 #f)) #f))]
     [(DirectDeclarator O_BRACKET TypeQualifierList? * C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 5) id (apply-type-context context (type:array (@ 2 5) #f #f $3 #f (id:star (@ 4 4)))) #f))]
     [(DirectDeclarator !PushFormalsContext !PushScope O_PAREN ParameterTypeList !PopDeclarator !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 9) id (apply-type-context context (type:function (@ 4 9) #f $5)) #f))]
     [(DirectDeclarator !PushFormalsContext !PushScope O_PAREN IdentifierList !PopDeclarator !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 9) id (apply-type-context context (type:function (@ 4 9) #f $5)) #f))]
     [(DirectDeclarator !PushFormalsContext !PushScope O_PAREN !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 7) id (apply-type-context context (type:function (@ 4 7) #f '())) #f))])

    ;; (declarator symbol type-context #f)
    (DirectDeclarator-tn
     [(!CacheDeclaratorIdentifier)
      (decl:declarator (@ 1) $1 #f #f)]
     [(O_PAREN Declarator-tn C_PAREN)
      $2]
     [(DirectDeclarator-tn O_BRACKET TypeQualifierList? AssignmentExpression? C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 5) id (apply-type-context context (type:array (@ 2 5) #f #f $3 $4 #f)) #f))]
     [(DirectDeclarator-tn O_BRACKET static TypeQualifierList? AssignmentExpression C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 6) id (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 3 3) 'static) $4 $5 #f)) #f))]
     [(DirectDeclarator-tn O_BRACKET TypeQualifierList static AssignmentExpression C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 6) id (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 4 4) 'static) $3 $5 #f)) #f))]
     [(DirectDeclarator-tn O_BRACKET TypeQualifierList? * C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 5) id (apply-type-context context (type:array (@ 2 5) #f #f $3 #f (id:star (@ 4 4)))) #f))]
     [(DirectDeclarator-tn !PushFormalsContext !PushScope O_PAREN ParameterTypeList !PopDeclarator !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 9) id (apply-type-context context (type:function (@ 4 9) #f $5)) #f))]
     [(DirectDeclarator-tn !PushFormalsContext !PushScope O_PAREN IdentifierList !PopDeclarator !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 9) id (apply-type-context context (type:function (@ 4 9) #f $5)) #f))]
     [(DirectDeclarator-tn !PushFormalsContext !PushScope O_PAREN !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 7) id (apply-type-context context (type:function (@ 4 7) #f '())) #f))])

    ;; (declarator symbol type-context #f)
    (DirectDeclarator-paren-tn
     [(!CacheDeclaratorIdentifier)
      (decl:declarator (@ 1) $1 #f #f)]
     [(!CacheDeclaratorTypedefName)
      (decl:declarator (@ 1) $1 #f #f)]
     [(O_PAREN Declarator-tn C_PAREN)
      $2]
     [(DirectDeclarator-paren-tn O_BRACKET TypeQualifierList? AssignmentExpression? C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 5) id (apply-type-context context (type:array (@ 2 5) #f #f $3 $4 #f)) #f))]
     [(DirectDeclarator-paren-tn O_BRACKET static TypeQualifierList? AssignmentExpression C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 6) id (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 3 3) 'static) $4 $5 #f)) #f))]
     [(DirectDeclarator-paren-tn O_BRACKET TypeQualifierList static AssignmentExpression C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 6) id (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 4 4) 'static) $3 $5 #f)) #f))]
     [(DirectDeclarator-paren-tn O_BRACKET TypeQualifierList? * C_BRACKET)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 5) id (apply-type-context context (type:array (@ 2 5) #f #f $3 #f (id:star (@ 4 4)))) #f))]
     [(DirectDeclarator-paren-tn !PushFormalsContext !PushScope O_PAREN ParameterTypeList !PopDeclarator !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 9) id (apply-type-context context (type:function (@ 4 9) #f $5)) #f))]
     [(DirectDeclarator-paren-tn !PushFormalsContext !PushScope O_PAREN IdentifierList !PopDeclarator !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 9) id (apply-type-context context (type:function (@ 4 9) #f $5)) #f))]
     [(DirectDeclarator-paren-tn !PushFormalsContext !PushScope O_PAREN !PopScope !PopContext C_PAREN)
      (match-let ([(struct decl:declarator (_ id context #f)) $1])
        (decl:declarator (@ 7) id (apply-type-context context (type:function (@ 4 7) #f '())) #f))])

    ;; (declarator (or symbol #f) type-context #f) -> (declarator (or symbol #f) type-context #f)
    ;; type-context -> type-context
    (Pointer
     [(* TypeQualifierList?)
      (lambda (context)
        (apply-type-context context (type:pointer (@ 2) #f $2)))]
;      (match-lambda
;        [(struct decl:declarator (src id context #f))
;         (decl:declarator (src-range src (@ 2)) id (apply-type-context context (type:pointer (@ 2) #f $2)) #f)])]
     [(* TypeQualifierList? Pointer)
      (lambda (body)
        (apply-type-context ($3 body) (type:pointer (@ 2) #f $2)))])
;      (lambda (body)
;        (match-let ([(struct decl:declarator (src id context #f)) ($3 body)])
;          (decl:declarator (src-range src (@ 3)) id (apply-type-context context (type:pointer (@ 2) #f $2)) #f)))])

    (TypeQualifierList
     [(TypeQualifier) (list $1)]
     [(TypeQualifierList TypeQualifier) (append $1 (list $2))])

    (TypeQualifierList?
     [() '()]
     [(TypeQualifierList) $1])

    (ParameterTypeList
     [(ParameterList) $1]
     [(ParameterList !PopParserDeclarator COMMA !PushDeclarator ELLIPSIS)
      (append $1 (list (id:ellipsis (@ 5 5))))])

    (ParameterList
     [(ParameterDeclaration)
      (list $1)]
     [(ParameterList !PopParserDeclarator COMMA ParameterDeclaration)
      (append $1 (list $4))])

    ;; XXX: need to change decl:formal completely-- should combine the abstract declarator and the decl-specs
    (ParameterDeclaration
     [(DeclarationModifiers !PushDeclarator Declarator-tn)
      (build-parameter-declaration $1 $3 (@ 3))]
     ;; XXX: document how this avoids an ambiguity
     [(DeclarationSpecifiers !PushDeclarator Declarator-paren-tn)
      (build-parameter-declaration $1 $3 (@ 3))]
     [(DeclarationSpecifiers !PushDeclarator AbstractDeclarator)
      (build-parameter-declaration $1 $3 (@ 3))]
     [(DeclarationModifiers !PushDeclarator AbstractDeclarator)
      (build-parameter-declaration $1 $3 (@ 3))]
     [(DeclarationModifiers !PushDeclarator)
      (build-parameter-declaration $1 #f (@ 2))]
     [(DeclarationSpecifiers !PushDeclarator)
      (build-parameter-declaration $1 #f (@ 2))])

    (IdentifierList
     [(!PushDeclarator !CacheDeclaratorIdentifier)
      (list $2)]
     [(IdentifierList !PopParserDeclarator COMMA !PushDeclarator !CacheDeclaratorIdentifier)
      (append $1 (list $5))])

    ;; XXX: needs to return a type!!
    (TypeName
     [(SpecifierQualifierList)
      (build-type-name $1 #f)]
;      $1]
     [(SpecifierQualifierList AbstractDeclarator)
      (build-type-name $1 $2)])
;      (append $1 (list $2))])

    (AbstractDeclarator
     [(Pointer)
      ($1 #f)]
;      ($1 (decl:declarator (@ 1) #f #f #f))]
     [(Pointer DirectAbstractDeclarator)
      ($1 $2)]
     [(DirectAbstractDeclarator)
      $1])

    ;; type-context
    (DirectAbstractDeclarator
     ;; XXX: replace this with the commented-out one below
     [(O_PAREN AbstractDeclarator C_PAREN)
      $2]
     #;[(O_PAREN !PushFormalsContext !PushScope AbstractDeclarator !PopScope !PopContext C_PAREN)
      $4]
     [(DirectAbstractDeclarator O_BRACKET TypeQualifierList AssignmentExpression? C_BRACKET)
      (apply-type-context $1 (type:array (@ 2 5) #f #f $3 $4 #f))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 5) #f (apply-type-context context (type:array (@ 2 5) #f #f $3 $4 #f)) #f))]
     [(DirectAbstractDeclarator O_BRACKET AssignmentExpression? C_BRACKET)
      (apply-type-context $1 (type:array (@ 2 4) #f #f $3 #f #f))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 4) #f (apply-type-context context (type:array (@ 2 4) #f #f $3 #f #f)) #f))]
     [(DirectAbstractDeclarator O_BRACKET static TypeQualifierList AssignmentExpression C_BRACKET)
      (apply-type-context $1 (type:array (@ 2 6) #f (id:storage (@ 3 3) 'static) $4 $5 #f))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 6) #f (apply-type-context context (type:array (@ 2 6) #f (id:storage (@ 3 3) 'static) $4 $5 #f)) #f))]
     [(DirectAbstractDeclarator O_BRACKET static AssignmentExpression C_BRACKET)
      (apply-type-context $1 (type:array (@ 2 5) #f (id:storage (@ 3 3) 'static) #f $4 #f))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 5) #f (apply-type-context context (type:array (@ 2 5) #f (id:storage (@ 3 3) 'static) #f $4 #f)) #f))]
     [(DirectAbstractDeclarator O_BRACKET TypeQualifierList static AssignmentExpression C_BRACKET)
      (apply-type-context $1 (type:array (@ 6) #f (id:storage (@ 4 4) 'static) $3 $5 #f))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 6) #f (apply-type-context context (type:array (@ 6) #f (id:storage (@ 4 4) 'static) $3 $5 #f)) #f))]
     [(DirectAbstractDeclarator O_BRACKET * C_BRACKET)
      (apply-type-context $1 (type:array (@ 2 4) #f #f #f #f (id:star (@ 3 3))))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 4) #f (apply-type-context context (type:array (@ 2 4) #f #f #f #f (id:star (@ 3 3)))) #f))]
     [(DirectAbstractDeclarator O_PAREN !PushFormalsContext !PushScope ParameterTypeList !PopDeclarator !PopScope !PopContext C_PAREN)
      (apply-type-context $1 (type:function (@ 2 9) #f $5))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 9) #f (apply-type-context context (type:function (@ 2 9) #f $5)) #f))]
     [(DirectAbstractDeclarator O_PAREN !PushFormalsContext !PushScope !PopScope !PopContext C_PAREN)
      (apply-type-context $1 (type:function (@ 2 7) #f '()))]
;      (match-let ([(struct decl:declarator (_ #f context #f)) $1])
;        (decl:declarator (@ 7) #f (apply-type-context context (type:function (@ 2 7) #f '())) #f))]
     [(O_BRACKET TypeQualifierList AssignmentExpression? C_BRACKET)
      (type:array (@ 4) #f #f $2 $3 #f)]
;      (decl:declarator (@ 4) #f (type:array (@ 4) #f #f $2 $3 #f) #f)]
     [(O_BRACKET AssignmentExpression? C_BRACKET)
      (type:array (@ 3) #f #f $2 #f #f)]
;      (decl:declarator (@ 3) #f (type:array (@ 3) #f #f $2 #f #f) #f)]
     [(O_BRACKET static TypeQualifierList AssignmentExpression C_BRACKET)
      (type:array (@ 5) #f (id:storage (@ 2 2) 'static) $3 $4 #f)]
;      (decl:declarator (@ 5) #f (type:array (@ 5) #f (id:storage (@ 2 2) 'static) $3 $4 #f) #f)]
     [(O_BRACKET static AssignmentExpression C_BRACKET)
      (type:array (@ 4) #f (id:storage (@ 2 2) 'static) #f $3 #f)]
;      (decl:declarator (@ 4) #f (type:array (@ 4) #f (id:storage (@ 2 2) 'static) #f $3 #f) #f)]
     [(O_BRACKET TypeQualifierList static AssignmentExpression C_BRACKET)
      (type:array (@ 5) #f (id:storage (@ 3 3) 'static) $2 $4 #f)]
;      (decl:declarator (@ 5) #f (type:array (@ 5) #f (id:storage (@ 3 3) 'static) $2 $4 #f) #f)]
     [(O_BRACKET * C_BRACKET)
      (type:array (@ 3) #f #f #f #f (id:star (@ 2 2)))]
;      (decl:declarator (@ 3) #f (type:array (@ 3) #f #f #f #f (id:star (@ 2 2))) #f)]
     [(O_PAREN !PushFormalsContext !PushScope ParameterTypeList !PopScope !PopContext C_PAREN)
      (type:function (@ 6) #f $4)]
;      (decl:declarator (@ 7) #f (type:function (@ 7) #f $4) #f)]
     [(O_PAREN !PushFormalsContext !PushScope !PopScope !PopContext C_PAREN)
      (type:function (@ 6) #f '())])
;      (decl:declarator (@ 6) #f (type:function (@ 6) #f '()) #f)])

    (Initializer
     [(AssignmentExpression)
      (init:expr (@ 1) $1)]
     [(O_BRACE InitializerList C_BRACE)
      (init:compound (@ 3) $2)]
     [(O_BRACE InitializerList COMMA C_BRACE)
      (init:compound (@ 4) $2)])

    ;; (listof 
    (InitializerList
     [(Designation Initializer)
      (list (cons $1 $2))]
     [(Initializer) (list $1)]
     [(InitializerList COMMA Designation Initializer)
      (append $1 (list (cons $3 $4)))]
     [(InitializerList COMMA Initializer)
      (append $1 (list $3))])

    (Designation
     [(DesignatorList =) $1])

    (DesignatorList
     [(Designator) (list $1)]
     [(DesignatorList Designator) (append $1 (list $2))])

    (Designator
     [(O_BRACKET ConstantExpression C_BRACKET)
      (dtor:array (@ 3) $2)]
     [(PERIOD IdentifierLabel)
      (dtor:member (@ 2) $2)]
     [(PERIOD TypedefNameLabel)
      (dtor:member (@ 2) $2)])


    ;; A.2.3 Statements

    (Statement
     [(LabeledStatement) $1]
     [(CompoundStatement) $1]
     [(ExpressionStatement) $1]
     [(SelectionStatement) $1]
     [(IterationStatement) $1]
     [(JumpStatement) $1])

    (Statement-tn
     [(LabeledStatement-tn) $1]
     [(CompoundStatement) $1]
     [(ExpressionStatement) $1]
     [(SelectionStatement) $1]
     [(IterationStatement) $1]
     [(JumpStatement) $1])

    ;; A statement that doesn't end in a one-armed if-statement.
    (Statement-onearmif
     [(LabeledStatement-onearmif) $1]
     [(CompoundStatement) $1]
     [(ExpressionStatement) $1]
     [(SelectionStatement-onearmif) $1]
     [(IterationStatement-onearmif) $1]
     [(JumpStatement) $1])

    (LabeledStatement
     [(IdentifierLabel COLON Statement) (stmt:label (@ 3) $1 $3)]
     [(TypedefNameLabel COLON Statement) (stmt:label (@ 3) $1 $3)]
     [(case ConstantExpression COLON Statement) (stmt:case (@ 4) $2 $4)]
     [(default COLON Statement) (stmt:default (@ 3) $3)])

    (LabeledStatement-tn
     [(IdentifierLabel COLON Statement) (stmt:label (@ 3) $1 $3)]
     [(case ConstantExpression COLON Statement) (stmt:case (@ 4) $2 $4)]
     [(default COLON Statement) (stmt:default (@ 3) $3)])

    (LabeledStatement-onearmif
     [(IdentifierLabel COLON Statement-onearmif) (stmt:label (@ 3) $1 $3)]
     [(TypedefNameLabel COLON Statement-onearmif) (stmt:label (@ 3) $1 $3)]
     [(case ConstantExpression COLON Statement-onearmif) (stmt:case (@ 4) $2 $4)]
     [(default COLON Statement-onearmif) (stmt:default (@ 3) $3)])

    (CompoundStatement
     [(!PushBlockContext !PushScope O_BRACE C_BRACE !PopScope !PopContext)
      (stmt:block (@ 6) '())]
     [(!PushBlockContext !PushScope O_BRACE BlockItemList C_BRACE !PopScope !PopContext)
      (stmt:block (@ 7) $4)])

    (BlockItemList
     [(BlockItem) (list $1)]
     [(BlockItemList BlockItem) (append $1 (list $2))])

    (BlockItem
     [(Declaration) $1]
     [(TypedefNameLabel !PushStatementContext COLON Statement !PopContext)
      (stmt:label (@ 5) $1 $4)]
     [(!PushStatementContext Statement-tn !PopContext) $2])

    (ExpressionStatement
     [(Expression SEMI_COLON)
      (stmt:expr (@ 2) $1)]
     [(SEMI_COLON)
      (stmt:empty (@ 1))])

    (SelectionStatement
     [(if O_PAREN Expression C_PAREN Statement-onearmif)
      (stmt:if (@ 5) $3 $5 #f)]
     [(if O_PAREN Expression C_PAREN Statement-onearmif else Statement)
      (stmt:if (@ 7) $3 $5 $7)]
     [(switch O_PAREN Expression C_PAREN Statement)
      (stmt:switch (@ 5) $3 $5)])

    (SelectionStatement-onearmif
     [(if O_PAREN Expression C_PAREN Statement-onearmif else Statement-onearmif)
      (stmt:if (@ 7) $3 $5 $7)]
     [(switch O_PAREN Expression C_PAREN Statement-onearmif)
      (stmt:switch (@ 5) $3 $5)])

    (IterationStatement
     [(while O_PAREN Expression C_PAREN Statement)
      (stmt:while (@ 5) $3 $5)]
     [(do Statement while O_PAREN Expression C_PAREN SEMI_COLON)
      (stmt:do (@ 7) $2 $5)]
     [(for !PushStatementContext !ForContext O_PAREN Expression? SEMI_COLON Expression? SEMI_COLON Expression? !PopContext C_PAREN Statement)
      (stmt:for (@ 12) $5 $7 $9 $12)]
     [(for !PushStatementContext !ForContext O_PAREN !PushScope Declaration Expression? SEMI_COLON Expression? !PopContext C_PAREN Statement !PopScope)
      (stmt:for (@ 13) $6 $7 $9 $12)])

    (IterationStatement-onearmif
     [(while O_PAREN Expression C_PAREN Statement-onearmif)
      (stmt:while (@ 5) $3 $5)]
     [(do Statement while O_PAREN Expression C_PAREN SEMI_COLON)
      (stmt:do (@ 7) $2 $5)]
     [(for !PushStatementContext !ForContext O_PAREN Expression? SEMI_COLON Expression? SEMI_COLON Expression? !PopContext C_PAREN Statement-onearmif)
      (stmt:for (@ 12) $5 $7 $9 $12)]
     [(for !PushStatementContext !ForContext O_PAREN !PushScope Declaration Expression? SEMI_COLON Expression? !PopContext C_PAREN Statement-onearmif !PopScope)
      (stmt:for (@ 13) $6 $7 $9 $12)])

    (JumpStatement
     [(goto IdentifierLabel SEMI_COLON)
      (stmt:goto (@ 3) $2)]
     [(goto TypedefNameLabel SEMI_COLON)
      (stmt:goto (@ 3) $2)]
     [(continue SEMI_COLON)
      (stmt:continue (@ 2))]
     [(break SEMI_COLON)
      (stmt:break (@ 2))]
     [(return Expression SEMI_COLON)
      (stmt:return (@ 3) $2)]
     [(return SEMI_COLON)
      (stmt:return (@ 2) #f)])

    ;; A.2.4 External Definitions

    (TranslationUnit
     [() '()]
     [(TranslationUnit ExternalDeclaration)
      (append $1 (list $2))])

    (ExternalDeclaration
     [(!PushBlockContext FunctionDefinition !PopContext) $2]
     [(!PushBlockContext Declaration !PopContext) $2])

    (FunctionDefinition
     [(FunctionDefinitionHead !PushPreambleContext DeclarationList? !PopContext CompoundStatement !PopScope !PopDeclarator)
      ;; XXX: check preamble matches head
      ($1 $3 $5)])

    (FunctionDefinitionHead
     [(DeclarationModifiers !PushDeclarator Declarator-tn)
      (begin
        (check-function-declarator! $3)
        (push-scope! ps)
        (add-function-bindings! $3 ps)
        (lambda (decls body)
          (build-function-definition $1 $3 decls body (src-range (@ 1 1) (stmt-src body)))))]
     [(DeclarationSpecifiers !PushDeclarator Declarator)
      (begin
        (check-function-declarator! $3)
        (push-scope! ps)
        (add-function-bindings! $3 ps)
        (lambda (decls body)
          (build-function-definition $1 $3 decls body (src-range (@ 1 1) (stmt-src body)))))])

    (DeclarationList?
     [() null]
     [(DeclarationList? Declaration)
      (append $1 (list $2))])

    )))

(define (input-source->input-port in)
  (cond
    [(path? in) (open-input-file in)]
    [(string? in) (open-input-string in)]
    [else in]))

(define (build-parser project context)
  (lambda (in #:typedef [typedef null] #:source [source #f] #:offset [offset #f])
    (let ([in (input-source->input-port in)])
      (port-count-lines! in)
      (let-values ([(ps ls) (new-state #f source offset (map (lambda (t) (cons t 'type)) typedef) context)])
        (let ([lex (c-lexer ps ls)]
              [parse (project (c-parser ps ls))])
          (parse (lambda () (lex in))))))))

(define parse-program (build-parser first 'block))
(define parse-declaration (build-parser second 'block))
(define parse-statement (build-parser third 'statement))
(define parse-expression (build-parser fourth 'expression))
(define parse-type-expression (build-parser fifth 'type-expression))

#;(define (parse-read in)
  (let ([in (input-source->input-port in)])
    (port-count-lines! in)
    (let-values ([(parser-state lexer-state) (new-state #t)])
      (let ([lexer (c-lexer parser-state lexer-state)]
            [parser (c-parser parser-state lexer-state)])
        (parser (lambda () (lexer in)))))))

#;(begin
  (require scheme/port)
  (when (file-exists? "debug.yacc.txt")
    (with-input-from-file "debug.yacc.txt"
      (lambda ()
        (copy-port (current-input-port) (current-error-port))))
    (delete-file "debug.yacc.txt")))
