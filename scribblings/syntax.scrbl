#lang scribble/doc

@require[scribble/manual
         scribble/eval
         scribble/basic
         scribblings/icons
         planet/scribble
         racket/list
         "abnf.rkt"
         "utils.rkt"]

@require[(for-label racket/base
                    racket/contract
                    racket/struct-info
                    parser-tools/lex
                    (this-package-in ast)
                    (this-package-in parse))]

@title[#:tag "syntax"]{The C Language}

This library provides data types representing C abstract syntax, a C parser,
and macros for constructing C abstract syntax with a convenient parenthesized syntax.
It can be required via:

@section[#:tag "grammar"]{C Language Grammar}

The grammar provided in the @link["http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf"]{ISO/IEC 9899:TC3}
standard is mum about when @italic{typedef-name} tokens can be used as @italic{identifier} tokens.
For example, all C parsers admit programs such as:
@verbatim{
    typedef int T;
    void proc(char T) { }}
despite the fact that @tt{T} is a @italic{typedef-name} and the grammar for procedure arguments
requires argument declarator names to be @italic{identifier} tokens.

The following is a more detailed (and slightly reorganized) grammar than the one in the C99 standard
which explicitly specifies when tokens bound as @tt{typedef} names can be used as identifiers.

@BNF[
(list @nonterm[#:sub @BNF-var{X}]{List}
      @BNF-seq[@BNF-var{X} @kleenestar[@BNF-group[@term[","] @nonterm[#:sub @BNF-var{X}]{List}]]])
(list @nonterm{AnyIdentifier}
      @nonterm{Identifier}
      @nonterm{TypedefName})
]

@subsection[#:tag "grammar_expressions"]{Expressions}

@begin[(define (binexp e . ops)
         @nonterm[#:sub @elem{(@nonterm[e],@(let ([ops (for/list ([op ops])
                                                         (if (string? op)
                                                             @term[op]
                                                             op))])
                                              (if (< (length ops) 2)
                                                  (apply elem ops)
                                                  (apply BNF-group (add-between ops "|")))))}]{BinaryExpression})]

@BNF[
(list @nonterm{PrimaryExpression}
      @nonterm{Identifier}
      @nonterm{Constant}
      @nonterm{StringLiteral}
      @BNF-seq[@term["("] @nonterm{Expression} @term[")"]])
(list @nonterm{PostfixExpression}
      @nonterm{PrimaryExpression}
      @BNF-seq[@nonterm{PostfixExpression} @term["["] @nonterm{Expression} @term["]"]]
      @BNF-seq[@nonterm{PostfixExpression} @term["("] @optional[@nonterm[#:sub @nonterm{AssignmentExpression}]{List}] @term[")"]]
      @BNF-seq[@nonterm{PostfixExpression} @term["."] @id-or-tn]
      @BNF-seq[@nonterm{PostfixExpression} @term["->"] @id-or-tn]
      @BNF-seq[@nonterm{PostfixExpression} @term["++"]]
      @BNF-seq[@nonterm{PostfixExpression} @term["--"]]
      @BNF-seq[@term["("] @nonterm{TypeName} @term[")"] @term["{"] @nonterm[#:sub @nonterm{Initializer}]{List} @optional[@term[","]] @term["}"]])
(list @nonterm{UnaryExpression}
      @BNF-seq[@term["++"] @nonterm{UnaryExpression}]
      @BNF-seq[@term["--"] @nonterm{UnaryExpression}]
      @BNF-seq[@BNF-group[@BNF-alt[@term["&"] @term["*"] @term["+"] @term["-"] @term["~"] @term["!"]]] @nonterm{CastExpression}]
      @BNF-seq[@term["sizeof"] @nonterm{UnaryExpression}]
      @BNF-seq[@term["sizeof"] @term["("] @nonterm{TypeName} @term[")"]])
(list @nonterm{CastExpression}
      @nonterm{UnaryExpression}
      @BNF-seq[@term["("] @nonterm{TypeName} @term[")"] @nonterm{CastExpression}])
(list @nonterm[#:sub @elem{(@BNF-var{E},@BNF-var{Op})}]{BinaryExpression}
      @BNF-var{E}
      @BNF-seq[@nonterm[#:sub @elem{(@BNF-var{E},@BNF-var{Op})}]{BinaryExpression} @BNF-var{Op} @BNF-var{E}])
(list @nonterm{MultiplicativeExpression}
      @binexp["CastExpression" "*" "/" "%"])
(list @nonterm{AdditiveExpression}
      @binexp["MultiplicativeExpression" "+" "-"])
(list @nonterm{ShiftExpression}
      @binexp["AdditiveExpression" "<<" ">>"])
(list @nonterm{RelationalExpression}
      @binexp["ShiftExpression" "<" ">" "<=" ">="])
(list @nonterm{EqualityExpression}
      @binexp["RelationalExpression" "==" "!="])
(list @nonterm{ANDExpression}
      @binexp["EqualityExpression" "&"])
(list @nonterm{ExclusiveORExpression}
      @binexp["ANDExpression" "^"])
(list @nonterm{InclusiveORExpression}
      @binexp["ExclusiveORExpression" "|"])
(list @nonterm{LogicalANDExpression}
      @binexp["LogicalORExpression" "||"])
(list @nonterm{ConditionalExpression}
      @nonterm{LogicalORExpression}
      @BNF-seq[@nonterm{LogicalORExpression} @term["?"] @nonterm{Expression} @term[":"] @nonterm{ConditionalExpression}])
(list @nonterm{AssignmentExpression}
      @nonterm{ConditionalExpression}
      @BNF-seq[@nonterm{UnaryExpression} @nonterm{AssignmentOperator} @nonterm{AssignmentExpression}])
(list @nonterm{AssignmentOperator}
      @BNF-alt[@term["="] @term["*="] @term["/="] @term["%="] @term["+="] @term["-="]]
      @BNF-alt[@term["<<="] @term[">>="] @term["&="] @term["^="] @term["|="]])
(list @nonterm{Expression}
      @nonterm[#:sub @nonterm{AssignmentExpression}]{List})
(list @nonterm{ConstantExpression}
      @nonterm{ConditionalExpression})
]

@subsection[#:tag "grammar_declarations"]{Declarations}

@BNF[
(list @nonterm{Declaration}
      @BNF-seq[@kleeneplus[@nonterm{DeclarationModifier}] @optional[@nonterm[#:sub @nonterm[#:sub id-only]{InitDeclarator}]{List}] @term[";"]]
      @BNF-seq[@nonterm{DeclarationSpecifiers} @optional[@nonterm[#:sub @nonterm[#:sub id-or-tn]{InitDeclarator}]{List}] @term[";"]])
(list @nonterm{DeclarationSpecifiers}
      @BNF-seq[@kleenestar[@nonterm{DeclarationModifier}]
               @nonterm{TaggedTypeSpecifier}
               @kleenestar[@nonterm{DeclarationModifier}]]
      @BNF-seq[@kleenestar[@nonterm{DeclarationModifier}]
               @nonterm{TypedefName}
               @kleenestar[@nonterm{DeclarationModifier}]]
      @BNF-seq[@kleenestar[@nonterm{DeclarationModifier}]
               @kleeneplus[@BNF-group[@nonterm{PrimTypeSpecifier} @kleenestar[@nonterm{DeclarationModifier}]]]])
(list @nonterm{DeclarationModifier}
      @BNF-seq[@nonterm{StorageClassSpecifier}]
      @BNF-seq[@nonterm{TypeQualifier}]
      @BNF-seq[@nonterm{FunctionSpecifier}])
(list @nonterm[#:sub @BNF-var{X}]{InitDeclarator}
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{Declarator} @optional[@BNF-seq[@term["="] @nonterm{Initializer}]]])
(list @nonterm{StorageClassSpecifier}
      @BNF-alt[@term["typedef"] @term["extern"] @term["static"] @term["auto"] @term["register"]])
(list @nonterm{TypeSpecifier}
      @nonterm{PrimTypeSpecifier}
      @nonterm{TaggedTypeSpecifier}
      @nonterm{TypedefName})
(list @nonterm{PrimTypeSpecifier}
      @BNF-alt[@term["void"]]
      @BNF-alt[@term["char"] @term["short"] @term["int"] @term["long"]]
      @BNF-alt[@term["float"] @term["double"]]
      @BNF-alt[@term["signed"] @term["unsigned"]]
      @BNF-alt[@term["_Bool"] @term["_Complex"]])
(list @nonterm{TaggedTypeSpecifier}
      @BNF-seq[@BNF-group[@BNF-alt[@term["struct"] @term["union"]]] @optional[@nonterm{Tag}] @term["{"] @kleeneplus[@nonterm{StructDeclaration}] @term["}"]]
      @BNF-seq[@BNF-group[@BNF-alt[@term["struct"] @term["union"]]] @nonterm{Tag}]
      @nonterm{EnumSpecifier})
(list @nonterm{Tag}
      @nonterm{Identifier}
      @nonterm{TypedefName})
(list @nonterm{StructDeclaration}
      @BNF-seq[@kleeneplus[@nonterm{TypeQualifier}] @optional[@nonterm[#:sub @nonterm[#:sub id-only]{StructDeclarator}]{List}] @term[";"]]
      @BNF-seq[@nonterm{StructSpecifiers} @optional[@nonterm[#:sub @nonterm[#:sub id-or-tn]{StructDeclarator}]{List}] @term[";"]])
(list @nonterm{StructSpecifiers}
      @BNF-seq[@kleenestar[@nonterm{TypeQualifier}]
               @nonterm{TaggedTypeSpecifier}
               @kleenestar[@nonterm{TypeQualifier}]]
      @BNF-seq[@kleenestar[@nonterm{TypeQualifier}]
               @nonterm{TypedefName}
               @kleenestar[@nonterm{TypeQualifier}]]
      @BNF-seq[@kleenestar[@nonterm{TypeQualifier}]
               @kleeneplus[@BNF-group[@nonterm{PrimTypeSpecifier} @kleenestar[@nonterm{TypeQualifier}]]]])
(list @nonterm[#:sub @BNF-var{X}]{StructDeclarator}
      @nonterm[#:sub @BNF-var{X}]{Declarator}
      @BNF-seq[@optional[@nonterm[#:sub @BNF-var{X}]{Declarator}] @term[":"] @nonterm{ConstantExpression}])
(list @nonterm{EnumSpecifier}
      @BNF-seq[@term["enum"] @optional[@nonterm{Tag}] @term["{"] @nonterm[#:sub @nonterm{Enumerator}]{List} @optional[@term[","]] @term["}"]]
      @BNF-seq[@term["enum"] @nonterm{Tag}])
(list @nonterm{Enumerator}
      @BNF-seq[id-or-tn @optional[@BNF-seq[@term["="] @nonterm{ConstantExpression}]]])
(list @nonterm{TypeQualifier}
      @BNF-alt[@term["const"] @term["restrict"] @term["volatile"]])
(list @nonterm{FunctionSpecifier}
      @term["inline"])
(list @nonterm[#:sub @BNF-var{X}]{Declarator}
      @BNF-seq[@optional[@nonterm{Pointer}] @nonterm[#:sub @BNF-var{X}]{DirectDeclarator}])
(list @nonterm[#:sub @BNF-var{X}]{DirectDeclarator}
      @BNF-seq[@italic{X}]
      @BNF-seq[@term["("] @nonterm[#:sub @BNF-var{X}]{Declarator} @term[")"]]
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @kleenestar[@nonterm{TypeQualifier}] @optional[@nonterm{AssignmentExpression}] @term["]"]]
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @term["static"] @kleenestar[@nonterm{TypeQualifier}] @nonterm{AssignmentExpression} @term["]"]]
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @kleeneplus{TypeQualifier} @term["static"] @nonterm{AssignmentExpression} @term["]"]]
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @kleenestar[@nonterm{TypeQualifier}] @term["*"] @term["]"]]
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["("] @nonterm{ParameterTypeList} @term[")"]]
      @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["("] @optional[@nonterm[#:sub @nonterm{Identifier}]{List}] @term[")"]])
(list @nonterm{Pointer}
      @kleeneplus[@BNF-group[@term["*"] @kleenestar[@nonterm{TypeQualifier}]]])
(list @nonterm{ParameterTypeList}
      @BNF-seq[@nonterm[#:sub @nonterm{ParameterDeclaration}]{List} @optional[@BNF-seq[@term[","] @term["..."]]]])
(list @nonterm{ParameterDeclaration}
      @BNF-seq[@kleeneplus[@nonterm{DeclarationModifier}] @optional[@nonterm[#:sub id-only]{Declarator}]]
      @BNF-seq[@nonterm{DeclarationSpecifiers} @optional[@nonterm[#:sub id-or-tn]{Declarator}]]
      @BNF-seq[@kleeneplus[@nonterm{DeclarationModifier}] @nonterm{AbstractDeclarator}]
      @BNF-seq[@nonterm{DeclarationSpecifiers} @nonterm{AbstractDeclarator}])
(list @nonterm{TypeName}
      @BNF-seq[@kleeneplus[@BNF-group[@BNF-alt[@nonterm{TypeSpecifier} @nonterm{TypeQualifier}]]] @optional[@nonterm{AbstractDeclarator}]])
(list @nonterm{AbstractDeclarator}
      @nonterm{Pointer}
      @BNF-seq[@optional[@nonterm{Pointer}] @nonterm{DirectAbstractDeclarator}])
(list @nonterm{DirectAbstractDeclarator}
      @BNF-seq[@term["("] @nonterm{AbstractDeclarator} @term[")"]]
      @BNF-seq[@optional[@nonterm{DirectAbstractDeclarator}] @term["["] @kleenestar[@nonterm{TypeQualifier}] @optional[@nonterm{AssignmentExpression}] @term["]"]]
      @BNF-seq[@optional[@nonterm{DirectAbstractDeclarator}] @term["["] @term["static"] @kleenestar[@nonterm{TypeQualifier}] @nonterm{AssignmentExpression} @term["]"]]
      @BNF-seq[@optional[@nonterm{DirectAbstractDeclarator}] @term["["] @kleeneplus[@nonterm{TypeQualifier}] @term["static"] @nonterm{AssignmentExpression} @term["]"]]
      @BNF-seq[@optional[@nonterm{DirectAbstractDeclarator}] @term["["] @term["*"] @term["]"]]
      @BNF-seq[@optional[@nonterm{DirectAbstractDeclarator}] @term["("] @optional[@nonterm{ParameterTypeList}] @term[")"]])
(list @nonterm{Initializer}
      @nonterm{AssignmentExpression}
      @BNF-seq[@term["{"] @nonterm[#:sub @BNF-group[@optional[@BNF-seq[@nonterm[#:sub @nonterm{Designator}]{List} @term["="]]] @nonterm{Initializer}]]{List} @optional[@term[","]] @term["}"]])
(list @nonterm{Designator}
      @BNF-seq[@term["["] @nonterm{ConstantExpression} @term["]"]]
      @BNF-seq[@term["."] id-or-tn])
]

@subsection[#:tag "grammar_statements"]{Statements}

The parameterized statement non-terminals such as @nonterm[#:sub @BNF-var{X}]{Statement}
take a flag indicating whether the productions may be right-terminated by a
one-armed @tt{if} statement (i.e., an @tt{if} statement with no @tt{else}
clause). This is used to avoid the ``@link["http://en.wikipedia.org/wiki/Dangling_else"]{dangling else}''
ambiguity.

@BNF[
(list @nonterm[#:sub @BNF-var{X}]{Statement}
      @nonterm[#:sub @BNF-var{X}]{LabeledStatement}
      @nonterm{CompoundStatement}
      @nonterm{ExpressionStatement}
      @nonterm[#:sub @BNF-var{X}]{SelectionStatement}
      @nonterm[#:sub @BNF-var{X}]{IterationStatement}
      @nonterm{JumpStatement})
(list @nonterm[#:sub @BNF-var{X}]{LabeledStatement}
      @BNF-seq[@nonterm{AnyIdentifier} @term[":"] @nonterm[#:sub @BNF-var{X}]{Statement}]
      @BNF-seq[@term["case"] @nonterm{ConstantExpression} @term[":"] @nonterm[#:sub @BNF-var{X}]{Statement}]
      @BNF-seq[@term["default"] @term[":"] @nonterm[#:sub @BNF-var{X}]{Statement}])
(list @nonterm{CompoundStatement}
      @BNF-seq[@term["{"] @kleenestar[@nonterm{BlockItem}] @term["}"]])
(list @nonterm{BlockItem}
      @nonterm{Declaration}
      @nonterm[#:sub @scheme[#t]]{Statement})
(list @nonterm{ExpressionStatement}
      @BNF-seq[@optional[@nonterm{Expression}] @term[";"]])
(list @nonterm[#:sub @BNF-var{X}]{SelectionStatement}
      @nonterm[#:sub @BNF-var{X}]{IfStatement}
      @BNF-seq[@term["switch"] @term["("] @nonterm{Expression} @term[")"] @nonterm[#:sub @BNF-var{X}]{Statement}])
(list @nonterm[#:sub @scheme[#t]]{IfStatement}
      @BNF-seq[@term["if"] @term["("] @nonterm{Expression} @term[")"] @nonterm[#:sub @scheme[#t]]{Statement} @optional[@BNF-seq[@term["else"] @nonterm[#:sub @scheme[#t]]{Statement}]]])
(list @nonterm[#:sub @scheme[#f]]{IfStatement}
      @BNF-seq[@term["if"] @term["("] @nonterm{Expression} @term[")"] @nonterm[#:sub @scheme[#t]]{Statement} @BNF-seq[@term["else"] @nonterm[#:sub @scheme[#f]]{Statement}]])
(list @nonterm[#:sub @BNF-var{X}]{IterationStatement}
      @BNF-seq[@term["while"] @term["("] @nonterm{Expression} @term[")"] @nonterm[#:sub @BNF-var{X}]{Statement}]
      @BNF-seq[@term["do"] @nonterm[#:sub @scheme[#t]]{Statement} @term["while"] @term["("] @nonterm{Expression} @term[")"] @term[";"]] ;@nonterm{DoStatement}
      @BNF-seq[@term["for"] @term["("] @optional[@nonterm{Expression}] @term[";"] @optional[@nonterm{Expression}] @term[";"] @optional[@nonterm{Expression}] @term[")"] @nonterm[#:sub @BNF-var{X}]{Statement}]
      @BNF-seq[@term["for"] @term["("] @nonterm{Declaration} @optional[@nonterm{Expression}] @term[";"] @optional[@nonterm{Expression}] @term[")"] @nonterm[#:sub @BNF-var{X}]{Statement}])
(list @nonterm{JumpStatement}
      @BNF-seq[@term["goto"] id-or-tn @term[";"]]
      @BNF-seq[@term["continue"] @term[";"]]
      @BNF-seq[@term["break"] @term[";"]]
      @BNF-seq[@term["return"] @optional[@nonterm{Expression}] @term[";"]])
]

@subsection[#:tag "grammar_programs"]{Programs}

@BNF[
(list @nonterm{TranslationUnit}
      @kleeneplus[@nonterm{ExternalDefinition}])
(list @nonterm{ExternalDefinition}
      @nonterm{FunctionDefinition}
      @nonterm{Declaration})
(list @nonterm{FunctionDefinition}
      @BNF-seq[@nonterm{FunctionHead} @optional[@nonterm[#:sub @nonterm{Declaration}]{List}] @nonterm{FunctionBody}])
(list @nonterm{FunctionHead}
      @BNF-seq[@kleeneplus[@nonterm{DeclarationModifier}] @nonterm[#:sub id-only]{Declarator}]
      @BNF-seq[@nonterm{DeclarationSpecifiers} @nonterm[#:sub id-or-tn]{Declarator}])
(list @nonterm{FunctionBody}
      @nonterm{CompoundStatement})
]

@section[#:tag "abstract_syntax"]{Abstract Syntax}

The abstract syntax of C is represented as structs. All of the structure definitions
are provided by the package

@defmodule/this-package[ast]

All of the structs defined in this library are
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{prefab}
@;seclink["prefab-struct" #:doc '(lib "scribblings/guide/guide.scrbl")]{prefab}
structs, and consist entirely of @scheme[read]-able and @scheme[write]-able data.

@subsection[#:tag "srcloc"]{Source Locations}

Source location information is stored with the following struct type.

@defstruct[src ([start-offset exact-nonnegative-integer?] [start-line exact-positive-integer?] [start-col exact-nonnegative-integer?]
                [end-offset exact-nonnegative-integer?] [end-line exact-positive-integer?] [end-col exact-nonnegative-integer?]
                [path any])]{}

@defproc[(src-start (src src?)) position?]{Extracts a source location's start position as a @scheme[position] struct.}

@defproc[(src-end (src src?)) position?]{Extracts a source location's end position as a @scheme[position] struct.}

@defproc[(build-src (start position?) (end position?) (path any)) src?]{Builds a source location struct from @scheme[position] structs.}

@defproc[(position-min (p position?) ...+) position?]{
Returns the least of the given positions @scheme[p], i.e. the @scheme[p] with the least @scheme[position-offset].}

@defproc[(position-max (p position?) ...+) position?]{
Returns the greatest of the given positions @scheme[p], i.e. the @scheme[p] with the greatest @scheme[position-offset].}

@defproc[(src-range (src src?) ...+) src?]{
Returns the smallest range that spans all the given source locations @scheme[src].}

@defproc[(src->syntax (x src?) (datum any '...) (original? boolean? #t)) syntax?]{
Converts a source location to a syntax object, using @scheme[datum] as the
syntax object's underlying datum. If @scheme[original?] is @scheme[#t], the
generated syntax object has the @scheme[syntax-original?] property.}

@defproc[(id->syntax (id id?) (original? boolean? #t)) syntax?]{
Converts an identifier to a syntax object, using the identifier name as
the syntax object's underlying datum. If @scheme[original?] is @scheme[#t], the
generated syntax object has the @scheme[syntax-original?] property.}

@; Note: Scribble is dropping the initial underscore as part of its italicization behavior.
@; See: http://list.cs.brown.edu/pipermail/plt-scheme/2009-March/031210.html
@defproc[(primitive-type-specifier? [x symbol?]) boolean?]{
Indicates whether @scheme[x] is a primitive type specifier, which is one of the symbols
@scheme['void], @scheme['char], @scheme['short], @scheme['int], @scheme['long],
@scheme['float], @scheme['double], @scheme['signed], @scheme['unsigned],
@scheme['__Bool], or @scheme['__Complex].}

@defproc[(unary-operator? [x symbol?]) boolean?]{
Indicates whether @scheme[x] is a unary operator symbol, which is one of the symbols
@scheme['&], @scheme['*], @scheme['+], @scheme['-], @scheme['~], or @scheme['!].}

@defproc[(binary-operator? [x symbol?]) boolean?]{
Indicates whether @scheme[x] is a binary operator symbol, which is one of the symbols
@scheme['*], @scheme['/], @scheme['%], @scheme['+], @scheme['-], @scheme['<<], @scheme['>>],
@scheme['<], @scheme['>], @scheme['<=], @scheme['>=], @scheme['==], @scheme['!=],
@scheme['&], @scheme['^], @scheme['\|], @scheme['&&], or @scheme['\|\|].}

@defproc[(assignment-operator? [x symbol?]) boolean?]{
Indicates whether @scheme[x] is an assignment operator symbol, which is one of the symbols
@scheme['=], @scheme['*=], @scheme['/=], @scheme['%=], @scheme['+=], @scheme['-=],
@scheme['<<=], @scheme['>>=], @scheme['&=], @scheme['^=], or @scheme['\|=].}

@defproc[(increment-operator? [x symbol?]) boolean?]{
Indicates whether @scheme[x] is an increment/decrement operator symbol, which is one of the symbols
@scheme['++] or @scheme['--].}

@subsection[#:tag "identifiers"]{Identifiers}

@defstruct[id ([src (or/c src? #f)])]{A C identifier, i.e., a variable name, type name, label name, or keyword.}

@defstruct[(id:var       id) ([name symbol?])]{A variable or type name.}
@defstruct[(id:label     id) ([name symbol?])]{A @tt{struct}, @tt{union}, or @tt{enum} tag, a statement label, or a @tt{struct} or @tt{union} member name.}
@defstruct[(id:qualifier id) ([name (or/c 'const 'restrict 'volatile)])]{A type qualifier.}
@defstruct[(id:op        id) ([name (or/c unary-operator? binary-operator? assignment-operator? increment-operator?)])]{A unary, binary, assignment, or increment/decrement operator.}
@defstruct[(id:storage   id) ([class (or/c 'typedef 'extern 'static 'auto 'register)])]{A storage class specifier.}
@defstruct[(id:inline    id) ()]{The @tt{inline} keyword.}
@defstruct[(id:ellipsis  id) ()]{The varargs keyword ``@tt{...}''.}
@defstruct[(id:star      id) ()]{The array-type modifier keyword ``@tt{*}''.}


@subsection[#:tag "expressions"]{Expressions}

@defstruct[expr ([src (or/c src? #f)])]{A C expression.}

@defstruct[(expr:ref            expr) ([id id:var?])]{A variable reference.}
@defstruct[(expr:int            expr) ([value integer?] [qualifiers (listof id:qualifier?)])]{An integer literal.}
@defstruct[(expr:float          expr) ([value inexact-real?] [qualifiers (listof id:qualifier?)])]{A floating-point literal.}
@defstruct[(expr:char           expr) ([source string?] [wide? boolean?])]{A character literal.
Character literals are stored as uninterpreted source, i.e., with escape sequences left in.
@examples[#:eval the-eval
          (parse-expression "'\\n'")]}
@defstruct[(expr:string         expr) ([source string?] [wide? boolean?])]{A string literal.
String literals are stored as uninterpreted source, i.e., with escape sequences left in.
@examples[#:eval the-eval
          (parse-expression "\"foo\\nbar\"")]}
@defstruct[(expr:compound       expr) ([type type?]
                                       [inits (listof (or/c init? (cons (listof dtor?) init?)))])]{A C99 @deftech{compound literal}.}
@defstruct[(expr:array-ref      expr) ([expr expr?] [offset expr?])]{An array dereference.}
@defstruct[(expr:call           expr) ([function expr?] [arguments (listof expr?)])]{A function call.}
@defstruct[(expr:member         expr) ([expr expr?] [label id:label?])]{A @tt{struct} or @tt{union} member dereference.}
@defstruct[(expr:pointer-member expr) ([expr expr?] [label id:label?])]{A @tt{struct}- or @tt{union}-pointer member dereference.}
@defstruct[(expr:postfix        expr) ([expr expr?] [op id:op?])]{
A postfix increment or decrement. The @scheme[id:op-name] field of @scheme[op] is an @scheme[increment-operator?] symbol.}
@defstruct[(expr:prefix         expr) ([op id:op?] [expr expr?])]{
A prefix increment or decrement. The @scheme[id:op-name] field of @scheme[op] is an @scheme[increment-operator?] symbol.}
@defstruct[(expr:cast           expr) ([type type?] [expr expr?])]{A type cast.}
@defstruct[(expr:sizeof         expr) ([term (or/c type? expr?)])]{A @tt{sizeof} expression.}
@defstruct[(expr:unop           expr) ([op id:op?] [expr expr?])]{
A unary operator expression. The @scheme[id:op-name] field of @scheme[op] is a @scheme[unary-operator?] symbol.}
@defstruct[(expr:binop          expr) ([left expr?]
                                       [op id:op?]
                                       [right expr?])]{
A binary operator expression. The @scheme[id:op-name] field of @scheme[op] is a @scheme[binary-operator?] symbol.}
@defstruct[(expr:assign         expr) ([left expr?]
                                       [op id:op?]
                                       [right expr?])]{
An assignment expression. The @scheme[id:op-name] field of @scheme[op] is an @scheme[assignment-operator?] symbol.}
@defstruct[(expr:begin          expr) ([left expr?] [right expr?])]{A sequence expression.}
@defstruct[(expr:if             expr) ([test expr?] [cons expr?] [alt expr?])]{A conditional expression.}


@subsection[#:tag "statements"]{Statements}

@defstruct[stmt ([src (or/c src? #f)])]{A C statement.}

@defstruct[(stmt:label    stmt) ([label id:label?] [stmt stmt?])]{A labeled statement.}
@defstruct[(stmt:case     stmt) ([expr expr?] [stmt stmt?])]{A @tt{case} statement.}
@defstruct[(stmt:default  stmt) ([stmt stmt?])]{A @tt{default} statement.}
@defstruct[(stmt:block    stmt) ([items (listof (or/c decl? stmt?))])]{A compound statement.}
@defstruct[(stmt:expr     stmt) ([expr expr?])]{An expression statement.}
@defstruct[(stmt:if       stmt) ([test expr?] [cons stmt?] [alt (or/c stmt? #f)])]{An @tt{if} statement.}
@defstruct[(stmt:switch   stmt) ([test expr?] [body stmt?])]{A @tt{switch} statement.}
@defstruct[(stmt:while    stmt) ([test expr?] [body stmt?])]{A @tt{while} statement.}
@defstruct[(stmt:do       stmt) ([body stmt?] [test expr?])]{A @tt{do}-@tt{while} statement.}
@defstruct[(stmt:for      stmt) ([init (or/c expr? decl? #f)]
                                 [test (or/c expr? #f)]
                                 [update (or/c expr? #f)]
                                 [body stmt?])]{A @tt{for} statement.}
@defstruct[(stmt:goto     stmt) ([label id:label?])]{A @tt{goto} statement.}
@defstruct[(stmt:continue stmt) ()]{A @tt{continue} statement.}
@defstruct[(stmt:break    stmt) ()]{A @tt{break} statement.}
@defstruct[(stmt:return   stmt) ([result (or/c expr? #f)])]{A @tt{return} statement.}
@defstruct[(stmt:empty    stmt) ()]{An empty statement.}


@subsection[#:tag "declarations"]{Declarations}

@defstruct[decl ([src (or/c src? #f)])]{A C declaration.}

@defstruct[(decl:typedef           decl) ([type type?]
                                          [declarators (listof declarator-context?)])]{
A type definition. Note that each of the @scheme[declarators] is a @tech{declarator context}.}
@defstruct[(decl:vars              decl) ([storage-class (or/c id:storage? #f)]
                                          [type (or/c type? #f)]
                                          [declarators (listof declarator-context?)])]{
A variable declaration. Note that each of the @scheme[declarators] is a @tech{declarator context}.}
@defstruct[(decl:formal            decl) ([storage-class (or/c id:storage? #f)]
                                          [type (or/c type? #f)]
                                          [declarator (or/c declarator-context? type-context?)])]{A formal argument declaration.}
@defstruct[(decl:function          decl) ([storage-class (or/c id:storage? #f)]
                                          [inline? (or/c id:inline? #f)]
                                          [return-type type?]
                                          [declarator declarator-context?]
                                          [preamble (or/c (listof decl?) #f)]
                                          [body stmt:block?])]{
A function definition.
Note that the @scheme[declarator] is a @tech{declarator context}.
The @tech{complete type} of the function can be obtained by applying
@scheme[apply-declarator-context] to the @scheme[return-type] and @scheme[declarator].}
@defstruct[(decl:declarator        decl) ([id (or/c id:var? #f)]
                                          [type (or/c type? #f)]
                                          [initializer (or/c init? #f)])]{
A @deftech{declarator}, i.e., a single variable binding within a variable declaration.}

There are two classes of declarator:
@defproc[(declarator-context? [x any]) boolean?]{
Determines whether @scheme[x] is a @deftech{declarator context}, which is a declarator with a @tech{type context} as its @scheme[type] field.}
@defproc[(complete-declarator? [x any]) boolean?]{
Determines whether @scheme[x] is a @deftech{complete declarator}, which is a declarator with a @tech{complete type} as its @scheme[type] field.}
@defstruct[(decl:member-declarator decl) ([id (or/c id:label? #f)]
                                          [type (or/c type? #f)]
                                          [initializer (or/c init? #f)]
                                          [bit-size (or/c expr? #f)])]{
A @deftech{member declarator}, i.e., a single member definition within a @tt{struct} or @tt{union} definition.}
                                                                      
There are two classes of member declarator:
@defproc[(member-declarator-context? [x any]) boolean?]{
Determines whether @scheme[x] is a @deftech{member declarator context}, which is a member declarator with a @tech{type context} as its @scheme[type] field.}
@defproc[(complete-member-declarator? [x any]) boolean?]{
Determines whether @scheme[x] is a @deftech{complete member declarator}, which is a member declarator with a @tech{complete type} as its @scheme[type] field.}
@defstruct[(decl:member            decl) ([type (or/c type? #f)]
                                          [declarators (listof decl:declarator?)])]{A member declaration within a @tt{struct} or @tt{union} definition.}


@subsection[#:tag "initializers"]{Initializers}

@defstruct[init ([src (or/c src? #f)])]{A C initializer.}

@defstruct[(init:compound init) ([elements (listof (or/c init? (cons (listof dtor?) init?)))])]{A C99 compound initializer.}
@defstruct[(init:expr     init) ([expr expr?])]{An expression initializer.}


@subsection[#:tag "designators"]{Designators}

@defstruct[dtor ([src (or/c src? #f)])]{A C99 designator.}

@defstruct[(dtor:array  dtor) ([expr expr?])]{An array designator.}
@defstruct[(dtor:member dtor) ([label id:label?])]{A @tt{struct} or @tt{union} member designator.}


@subsection[#:tag "types"]{Types}

@defstruct[type ([src (or/c src? #f)])]{A C type.}

@defstruct[(type:primitive type) ([name (or/c primitive-type-specifier? (listof primitive-type-specifier?))])]{
A primitive type. The @scheme[name] field can be one of:
@itemize[
  @item{@scheme['void], @scheme['char], @scheme['short], @scheme['int], @scheme['long], @scheme['float], @scheme['double], @scheme['signed], @scheme['unsigned], @scheme['__Bool], or @scheme['__Complex]}
  @item{@scheme['(signed char)]}
  @item{@scheme['(unsigned char)]}
  @item{@scheme['(signed short)]}
  @item{@scheme['(signed short int)]}
  @item{@scheme['(unsigned short)]}
  @item{@scheme['(unsigned short int)]}
  @item{@scheme['(signed int)]}
  @item{@scheme['(unsigned int)]}
  @item{@scheme['(signed long)]}
  @item{@scheme['(long int)]}
  @item{@scheme['(signed long int)]}
  @item{@scheme['(unsigned long)]}
  @item{@scheme['(unsigned long int)]}
  @item{@scheme['(long long)]}
  @item{@scheme['(signed long long)]}
  @item{@scheme['(long long int)]}
  @item{@scheme['(signed long long int)]}
  @item{@scheme['(unsigned long long)]}
  @item{@scheme['(unsigned long long int)]}
  @item{@scheme['(long double)]}
  @item{@scheme['(float __Complex)]}
  @item{@scheme['(double __Complex)]}
  @item{@scheme['(long double __Complex)]}
]}
@defstruct[(type:ref       type) ([id id:var?])]{A reference to a @tt{typedef} name.}
@defstruct[(type:struct    type) ([tag id:label?] [fields (or/c (listof decl:member?) #f)])]{A @tt{struct} type.}
@defstruct[(type:union     type) ([tag id:label?] [variants (or/c (listof decl:member?) #f)])]{A @tt{union} type.}
@defstruct[(type:enum      type) ([tag id:label?] [variants (or/c (listof (or/c id:var? (cons id:var? expr?))) #f)])]{An @tt{enum} type.}
@defstruct[(type:array     type) ([base type?]
                                  [static? (or/c id:static? #f)]
                                  [qualifiers (listof id:qualifier?)]
                                  [length (or/c expr? #f)]
                                  [star? (or/c id:star? #f)])]{An @tt{array} type.}
@defstruct[(type:pointer   type) ([base type?] [qualifiers (listof id:qualifier?)])]{A pointer type.}
@defstruct[(type:function  type) ([return type?] [formals (listof (or/c decl:formal? id:ellipsis?))])]{A function type.}
@defstruct[(type:qualified type) ([type (or/c type? #f)] [qualifiers (listof id:qualifier?)])]{A qualified type.}

@subsection[#:tag "type_contexts"]{Type Contexts}

The peculiar syntax of declarations in C leads to a particular notion of @deftech{type context}.
A type context is a @scheme[type] with a ``hole,'' represented by the value @scheme[#f]. For
example, in the C declaration
@verbatim{
typedef int A[32], *PA[32];
}
there are two declared types, @tt{A} and @tt{PA}, each of which is formed by plugging the base
type @tt{int} into the respective type contexts @tt{__[32]} and @tt{*__[32]}.

More precisely, a type context is one of:
@itemize[
    @item{@scheme[#f]@";"}
    @item{a @scheme[type:pointer] whose @scheme[type:pointer-base] field is a type context@";"}
    @item{a @scheme[type:array] whose @scheme[type:array-base] field is a type context@";"}
    @item{or a @scheme[type:function] whose @scheme[type:function-return] field is a type context.}
]
@defproc[(type-context? [x any]) boolean?]{Determines whether @scheme[x] is a type context.}

A @deftech{complete type} is a @scheme[type] with no holes.
@defproc[(complete-type? [x any]) boolean?]{Determines whether @scheme[x] is a complete type.}

@defproc[(apply-type-context [context type-context?] [base complete-type?]) complete-type?]{
Plugs the type @scheme[base] into the hole of @scheme[context] to obtain a complete type.}

@defproc[(apply-declarator-context [context declarator-context?] [base complete-type?]) complete-declarator?]{
Plugs the type @scheme[base] into the hole of @scheme[context] to obtain a @tech{complete declarator}.}

@defproc[(apply-declarator-contexts [contexts (listof declarator-context?)] [base complete-type?]) (listof complete-declarator?)]{
Plugs the type @scheme[base] into each of the holes of @scheme[contexts] to obtain a list of @tech{complete declarator}s.}

@defproc[(apply-member-declarator-context [context declarator-context?] [base complete-type?]) complete-member-declarator?]{
Plugs the type @scheme[base] into the hole of @scheme[context] to obtain a @tech{complete member declarator}.}

@defproc[(apply-member-declarator-contexts [contexts (listof member-declarator-context?)] [base complete-type?])
         (listof complete-member-declarator?)]{
Plugs the type @scheme[base] into each of the holes of @scheme[contexts] to obtain a list of @tech{complete member declarator}s.}
