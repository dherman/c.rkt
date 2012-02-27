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
                    racket/include
                    parser-tools/lex
                    (this-package-in ast)
                    (this-package-in parse))]

@title[#:tag "parsing"]{Parsing and Reading C}

@defmodule/this-package[parse]

Each of the parsing operations takes three optional keyword arguments. The @scheme[#:typedef]
argument takes a list of type names to bind as though by @tt{typedef} in the parser's initial
environment. The @scheme[#:source] argument is used to identify the source of the input in the
source location information. The @scheme[#:offset] argument is used as the base offset of
source location information. This is useful for parsing source extracted from the middle of
a file or input port. If this argument is ommitted or @scheme[#f], the parsed text is assumed to
appear at the beginning of the input source.

@section[#:tag "parse"]{Parsing}

@defproc[(parse-program [in (or/c input-port? string? path?)] [#:typedef typedef (or/c (listof symbol?) #f) #f] [#:source source any #f] [#:offset offset (or/c position? #f) #f]) (listof decl?)]{Parses a C program (@nonterm{TranslationUnit}).}
@defproc[(parse-declaration [in (or/c input-port? string? path?)] [#:typedef typedef (or/c (listof symbol?) #f) #f] [#:source source any #f] [#:offset offset (or/c position? #f) #f]) decl?]{Parses a C declaration (@nonterm{ExternalDeclaration}).}
@defproc[(parse-statement [in (or/c input-port? string? path?)] [#:typedef typedef (or/c (listof symbol?) #f) #f] [#:source source any #f] [#:offset offset (or/c position? #f) #f]) stmt?]{Parses a C statement (@nonterm{Statement}).}
@defproc[(parse-expression [in (or/c input-port? string? path?)] [#:typedef typedef (or/c (listof symbol?) #f) #f] [#:source source any #f] [#:offset offset (or/c position? #f) #f]) expr??]{Parses a C expression (@nonterm{Expression}).}
@defproc[(parse-type-expression [in (or/c input-port? string? path?)] [#:typedef typedef (or/c (listof symbol?) #f) #f] [#:source source any #f] [#:offset offset (or/c position? #f) #f]) type?]{Parses a C type expression (@nonterm{TypeName}).}

@section[#:tag "embedding"]{Embedding C in Scheme}

@margin-note{@finger See @other-manual['(lib "scribblings/scribble/scribble.scrbl")] for more information about Scribble.}

This library includes macros for embedding C source in Scheme to be
parsed at compile time. These macros are designed to work with the
@seclink["reader" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{Scribble @"@"-reader}
to make this embedding convenient.

The following example defines a list of C declaration nodes, parsed
at compile time and bound at runtime to @schemeid[decls]:

@schememod[
at-exp scheme/base
@code:comment{ ...}
(define decls
  #, @schemekeywordfont|{@}|#, @scheme[program]#, @schemeparenfont|{[}|#, @scheme[#:typedef (word)]#, @schemeparenfont|{]}|#, @schemeparenfont|{{}|
  #, @schemefont|{    struct tm {}|
  #, @schemefont|{        word tm_sec;}|
  #, @schemefont|{        word tm_min;}|
  #, @schemefont|{        word tm_hour;}|
  #, @schemefont|{        word tm_mday;}|
  #, @schemefont|{        word tm_mon;}|
  #, @schemefont|{        word tm_year;}|
  #, @schemefont|{        word tm_wday;}|
  #, @schemefont|{        word tm_yday;}|
  #, @schemefont|{        word tm_isdst;}|
  #, @schemefont|{    };}|
  #, @schemeparenfont|{}}|)]

Notice the use of the @seclink["at-exp-lang" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{@schememodname[at-exp]}
language to add @"@"-reader support to the specified @scheme[scheme/base] language.

@subsection[#:tag "scribble"]{Scribble Reader}

The Scribble @"@"-reader does not itself recognize C syntax;
it simply treats free-form text enclosed between @schemeparenfont["{"] @schemeparenfont["}"]
pairs as string literals. It does match delimiters such as braces, parentheses
and brackets, however, which works well with the syntax of C.

The special forms defined in this library work in conjunction with the
Scribble reader to allow embedding C as string literals. The reader simply
determines the end of the input, and parsing occurs as part of macro expansion.

The forms defined in this library accept only string literals, so in particular nested
@"@"-expressions are disallowed.

Since the syntax of C requires parsers to maintain a type environment to distinguish
variable names and type names (as bound by @tt{typedef}), all type names must be declared
before they are used. For convenience, each of the special forms in this library accepts
an optional @scheme[#:typedef] argument to pre-declare type names.

@subsection[#:tag "here"]{Avoid Using Here Strings}

Technically, it is possible to use the special forms in this library without the
Scribble reader by using
@seclink["parse-string" #:doc '(lib "scribblings/reference/reference.scrbl")]{here strings}
instead. However, the library is unable to extract accurate source location information with
here strings, so this is not recommended.

@subsection[#:tag "embedding_forms"]{Embedding C}

@defform*[[(program #:typedef (type-id ...) src-string ...+)
           (program src-string ...+)]]{
Parses the concatenated @scheme[src-string] fragments with @scheme[parse-program]
and expands into a list of @scheme[decl] nodes.}

@defform*[[(declaration #:typedef (type-id ...) src-string ...+)
           (declaration src-string ...+)]]{
Parses the concatenated @scheme[src-string] fragments with @scheme[parse-declaration]
and expands into a @scheme[decl] node.}

@defform*[[(statement #:typedef (type-id ...) src-string ...+)
           (statement src-string ...+)]]{
Parses the concatenated @scheme[src-string] fragments with @scheme[parse-statement]
and expands into a @scheme[stmt] node.}

@defform*[[(expression #:typedef (type-id ...) src-string ...+)
           (expression src-string ...+)]]{
Parses the concatenated @scheme[src-string] fragments with @scheme[parse-expression]
and expands into an @scheme[expr] node.}

@defform*[[(type-expression #:typedef (type-id ...) src-string ...+)
           (type-expression src-string ...+)]]{
Parses the concatenated @scheme[src-string] fragments with @scheme[parse-type-expression]
and expands into an @scheme[type] node.}

@section[#:tag "include"]{Including C Externally}

@defproc[(make-program-reader [#:typedef typedef (listof symbol?) '()]) (any input-port? -> (or/c (listof decl?) eof-object?))]{
Produces a procedure that reads an entire input port as a C program.
The resulting procedure is suitable for use as the expansion-time argument
to @scheme[include/reader].}

Using @scheme[include/reader], this makes it relatively convenient to include C source
code from an external file:

@schemeblock[(require scheme/include
                      (for-syntax scheme/base)
                      (for-syntax #, @schememodname/this-package[parse]))
             @code:comment{ ...}
             (define fnord.h
               (include/reader
                "fnord.h"
                (make-program-reader #:typedef '(BOOL WORD UINT32))))]

Notice that the relevant bindings must be available in the transformer environment in order
to apply @scheme[make-program-reader] in the expansion-time argument to @scheme[include/reader].
