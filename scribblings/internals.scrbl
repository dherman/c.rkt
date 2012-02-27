#lang scribble/doc

@require[scribble/manual
         scribble/eval
         scribble/basic
         planet/scribble
         "abnf.rkt"
         "utils.rkt"]

@require[(for-label racket/base
                    racket/contract
                    parser-tools/lex
                    parser-tools/yacc
                    (except-in (this-package-in ast) declarator-context? type-context?)
                    (this-package-in private/syntactic-context)
                    (this-package-in private/lexer)
                    (this-package-in private/parser))]

@title[#:tag "internals"]{Internals}

This section provides some information about the implementation of this package. It is not
required at all to understand how to use any of the C Metaprogramming Utilities.
@bold{None of the modules documented in this section should be used externally, and are subject to incompatible changes at any time.}

@section[#:tag "internals_parser"]{Parser}

C is not an easy language to parse. The grammar is essentially
@link["http://en.wikipedia.org/wiki/LALR_parser"]{LALR(1)}, except for one
nasty wrinkle: there are two lexical classes that are considered distinct in the grammar
but whose lexical definitions are identical: @italic{identifier} and @italic{typedef_name}.

The language distinguish these two classes by their bindings, so parsers must maintain an
environment and make the environment available to the lexer. However, because the grammar
is LALR(1), the lexer may sometimes look ahead of the current production(s) being parsed.
Consequently, the lexer must also make sure that it does not look ahead to an apparent
identifier before committing all necessary updates to the environment.

For example, in a program such as:
@verbatim{
    typedef int T;
    T a@";"}
the second occurrence of @tt{T} might be tokenized during lookahead before the parser
has had a chance to update the the environment from the first declaration. As a result,
the lexer must be careful whenever it reaches the end of a potential declaration to
commit any outstanding updates to the environment.

The following articles were useful in developing the parser:

@itemize[
@item{@link["http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf"]{ISO/IEC 9899:TC3}---the C99 standard.}
@item{@link["http://www.informit.com/guides/content.aspx?g=cplusplus&seqNum=215"]{A Tour of C99}---a 2004 article
      on some of the new, exotic features of C99.}
@item{@link["http://www.ddj.com/linux-open-source/196603535"]{Practical Parsing for ANSI C}---an article in
      @link["http://www.ddj.com"]{Dr. Dobb's Journal} with a few helpful tips on parsing C. It has some errors,
      however, and shouldn't be considered authoritative.}
@item{@link["http://groups.google.com/group/comp.compilers/msg/c0797b5b668605b4"]{Parsing C, the last word} from
      @link["http://groups.google.com/group/comp.compilers"]{@tt{comp.compilers}}---a message from
      Jim Roskind back in 1992 with some tips on tracking @tt{typedef} bindings.}
@item{@link["http://groups.google.com/group/comp.compilers/msg/9d8a968f346d24c0"]{Parsing C typedefs} from
      @link["http://groups.google.com/group/comp.compilers"]{@tt{comp.compilers}}---a message from
      Dale R. Worley back in 1992 with excellent advice on how to allow @tt{typedef} names in certain
      identifier contexts without generating LALR conflicts.}
]

@subsection[#:tag "grammar_definitions"]{Grammar Definitions}

A @deftech{declarator terminator} is a token of @scheme[token-name] @scheme['COMMA], @scheme['=], or @scheme['SEMI_COLON].

A @deftech{declarator id} is the value of the @attr-label[DeclaratorId] attribute of a @nonterm{Declarator} node,
using the following attribution rules for the language grammar:

@ABNF[
(list @nonterm[#:sub @BNF-var{X}]{Declarator}
      (list @BNF-seq[@optional[@nonterm{Pointer}] @nonterm[#:sub @BNF-var{X}]{DirectDeclarator}]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]]))
(list @nonterm[#:sub @BNF-var{X}]{DirectDeclarator}
      (list @BNF-seq[@italic{X}]
            @attr-decl[0 DeclaratorId @node-var[1]])
      (list @BNF-seq[@term["("] @nonterm[#:sub @BNF-var{X}]{Declarator} @term[")"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]])
      (list @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @kleenestar[@nonterm{TypeQualifier}] @optional[@nonterm{AssignmentExpression}] @term["]"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]])
      (list @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @term["static"] @kleenestar[@nonterm{TypeQualifier}] @nonterm{AssignmentExpression} @term["]"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]])
      (list @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @kleeneplus{TypeQualifier} @term["static"] @nonterm{AssignmentExpression} @term["]"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]])
      (list @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["["] @kleenestar[@nonterm{TypeQualifier}] @term["*"] @term["]"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]])
      (list @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["("] @nonterm{ParameterTypeList} @term[")"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]])
      (list @BNF-seq[@nonterm[#:sub @BNF-var{X}]{DirectDeclarator} @term["("] @optional[@nonterm[#:sub @nonterm{Identifier}]{List}] @term[")"]]
            @attr-decl[0 DeclaratorId @attr-sel[1 DeclaratorId]]))
]

@subsection[#:tag "grammar_invariants"]{Grammar Invariants}

Following are some invariants---essentially, informal lemmas---about the implemented grammar:

@itemize[
@item{Every @nonterm{Declarator} has a @tech{declarator id}.}
@item{In a @nonterm{Declaration} or @nonterm{StructDeclaration} production, a @nonterm{TypedefName} can be used as a @tech{declarator id} iff no @nonterm{TypeSpecifier} precedes it in the declaration.}
@item{In a valid program, the token immediately following a @nonterm{Declarator} is always a @tech{declarator terminator}.}
]


@subsection[#:tag "syntactic_context"]{Syntactic Contexts}

The internal module @schememodname/this-package[private/syntactic-context]
@;schememodname["private/syntactic-context.ss"]
defines structures that are used internally
to maintain information about parsing and lexing state. It could be accessed via

@defmodule/this-package[private/syntactic-context]

but generally shouldn't be used.

@defstruct[lexer-state ([read? boolean?]
                        [source any]
                        [offset (or/c position? #f)]
                        [declarators (listof (box/c (or/c symbol? #f)))]
                        [brace-depth exact-nonnegative-integer?]
                        [parenthesis-depth exact-nonnegative-integer?]
                        [previous-token (or/c token? #f)])]{
State pertaining to the lexer.

The @scheme[read?] field is used by the lexer for implementing a C reader.
When @scheme[read?] is @scheme[#t], the lexer reports an @scheme[EOF] token when it reaches
a terminating delimiter.

The @scheme[source] field is used to identify the input source, such as with a path. The
@scheme[offset] is used as a base offset for generating source location information. When
@scheme[offset] is @scheme[#f] the base location @scheme[(make-position 1 1 0)] is used.

The @scheme[declarators] stack contains the cache for the current @tech{declarator id} if the current
context is a @tech{declaration context}. Both the lexer state and parser state maintain views of
the declarators stack, which can be compared to determine whether the lexer is currently looking
ahead. The @scheme[car] of the stack is the @deftech{current declarator cache}.

The @scheme[brace-depth] tracks the nesting depth of curly-brace delimiters, and the @scheme[parenthesis-depth]
tracks the nesting depth of parenthesis delimiters.

The @scheme[previous-token] stores the previously lexed token or @scheme[#f] if no tokens have been read yet.}

A @deftech{major context} is one of
@itemize[
@item{@scheme['block]---a block context, such as the top-level of a program or compound statement.}
@item{@scheme['formals]---the formal arguments to a function definition or function signature.}
@item{@scheme['preamble]---the preamble to a function definition body, i.e., the declarations preceding the function body in a K & R-style function definition.}
@item{@scheme['struct]---the body of a @tt{struct} type.}
@item{@scheme['union]---the body of a @tt{union} type.}
@item{@scheme['enum]---the body of an @tt{enum} type.}
@item{@scheme['statement]---a non-declaration context such as a statement or expression.}
]

A @deftech{minor context} is one of
@itemize[
@item{@scheme['typedef]---a @tt{typedef} declaration in a @scheme['block] major context.}
@item{@scheme['for]---a @tt{for} statement in a @scheme['statement] major context.}
@item{@scheme[#f]---anything else.}
]

@defstruct[parser-state ([env (listof (listof (cons symbol (or/c 'var 'type))))]
                         [declarators (listof (box/c (or/c symbol? #f)))]
                         [major-context (listof #, @tech{major context})]
                         [minor-context (listof #, @tech{minor context})])]{
State pertaining to the parser. The @scheme[env] field is the current environment.

The @scheme[declarators] stack contains the parser's view of the same cache as the
@scheme[lexer-state-declarators] stack.

The @scheme[major-context] stack tracks the @tech{major context}. The @scheme[car] of
the stack is the @deftech{current major context}. The current major context is a
@deftech{declaration context} if it is one of @scheme['block], @scheme['formals], or
@scheme['preamble].

The @scheme[minor-context] stack tracks the @tech{minor context}. The @scheme[car] of
the stack is the @deftech{current minor context}. A minor context is a
@deftech{typedef context} if its value is @scheme['typedef].}

@subsection[#:tag "parser_invariants"]{Lexer and Parser Invariants}

Following are some invariants about the state of the lexer and parser.

@itemize[
@item{After an @nonterm{InitDeclaratorList} there is one extra @tech{declarator id} left in both the
@scheme[lexer-state-declarators] and @scheme[parser-state-declarators] stack.}
@item{In a @tech{declaration context}, when the lexer reaches a @tech{declarator terminator} it pops the @scheme[lexer-state-declarators] stack.}
@item{In a @tech{declaration context}, the @tech{declarator id} of every @nonterm{Declarator} is either cached by the parser before the lexer reaches the next @tech{declarator terminator} or immediately precedes the @tech{declarator terminator} token.}
@item{In a @tech{declaration context}, when the parser reaches a @tech{declarator terminator} it adds the saved @tech{declarator id} to the environment.}
@item{In a @tech{declaration context}, the @tech{declarator id} is added to the current environment when the lexer reaches a @tech{declarator terminator}.}
@item{In a non-@tech{declaration context}, @tech{declarator id}'s are not cached, nor is the current environment frame ever extended.}
@item{In a @tech{declaration context}, if the lexer reaches a @tech{declarator terminator} and the @scheme[parser-state-declarators] and @scheme[lexer-state-declarators] are the same, then the parser has not reduced the preceding @nonterm{Declarator}, i.e., the lexer is looking ahead.}
@item{In a @tech{declaration context}, if the parser reduces a @nonterm{Declarator} and the @scheme[parser-state-declarators] and @scheme[lexer-state-declarators] are the same, then the lexer has not reached the @tech{declarator terminator}, i.e., the lexer has not looked ahead.}
@item{The lexer never reads more than one token ahead of the innermost production preceding the parser's current position in the grammar.}
]

@subsection[#:tag "typedef_names_as_ids"]{Type Names as Identifiers}

A popular but non-standard extension to C is to allow @nonterm{TypedefName} tokens to be
used in many of the contexts that expect an @nonterm{Identifier}. For example, a variable
declaration may shadow an existing name that was already bound by @tt{typedef}:

@verbatim{
    typedef int T;
    void foo(char T) { }}

However, these extensions do result in one esoteric ambiguity: in a parameter declaration
such as

@verbatim{typedef void (*fn)(int (T));}

it is possible to interpret the parenthesized @nonterm{TypedefName} @tt{T} as either a
parenthesized declaration---i.e., an @tt{int} parameter, named @tt{T}, to the @tt{fn}
function type---or a nested function parameter list---i.e., an anonymous parameter of
type @tt{T} to a function type of return type @tt{int}.

To be a conservative extension of standard C, however, the ambiguity must be resolved
with the latter interpretation: the token @tt{T} is treated as a type name, not an
identifier.

The implementation of this resolution actually requires forking the definition of
@nonterm{DirectDeclarator} to provide an alternative version whose parenthesized
production does not allow occurrences of @nonterm{TypedefName}.


@section[#:tag "internals_header"]{Header Compilation}

The compilation of headers uses a curious computational idiom of two separate
monads in two distinct stages. The first monadic pass (``precompilation'')
generates a computation in the second monadic pass (``compilation'').

More to follow.
