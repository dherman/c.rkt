#lang scribble/doc

@require[scribble/manual
         scribble/eval
         scribble/basic
         planet/scribble
         "utils.rkt"]

@require[(for-label racket/base
                    racket/contract
                    racket/include
                    (only-in scribblings/foreign/unsafe-foreign ptr-ref ptr-set!)
                    (this-package-in header)
                    (this-package-in eval)
                    (this-package-in parse)
                    (this-package-in ast)
                    (this-package-in pc))]

@title[#:tag "header"]{Header Compilation}

This library provides facilities for extracting and compiling C header
information to generate architecture-specific binary layout information.
This is particularly useful for writing code that interacts with the PLT
Scheme foreign library---see @other-manual['(lib "scribblings/foreign/foreign.scrbl")].

Specifically, the foreign library's pointer-manipulation procedures such as
@scheme[ptr-ref] and @scheme[ptr-set!] can be used to read and write to arbitrary
addresses, which can be computed using layout information.

@defmodule/this-package[header]

@section[#:tag "headers"]{Headers}

@defproc[(header? [x any]) boolean?]{Determines whether @scheme[x] is a header.}
@defproc[(make-header [components (listof (or/c decl? header?))]) header?]{Constructs a header.}
@defproc[(header [component (or/c decl? header?)] ...+) header?]{Constructs a header.}

@examples[#:eval the-eval
          (define example.h
            (make-header (include/reader "example.h"
                                         (make-program-reader))))
          (define time.h
            (header
             (struct tm ([int tm_sec]
                         [int tm_min]
                         [int tm_hour]
                         [int tm_mday]
                         [int tm_mon]
                         [int tm_year]
                         [int tm_wday]
                         [int tm_yday]
                         [int tm_isdst]))))]

Using the @seclink["reader" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{Scribble @"@"-reader},
the latter example could be rewritten with @seclink["embedding"]{embedded C syntax} as:

@schemeblock[
(define time.h
  (make-header
   #, @schemekeywordfont|{@}|#, @scheme[program]#, @schemeparenfont|{{}|
   #, @schemefont|{  struct tm {}|
   #, @schemefont|{      int tm_sec;}|
   #, @schemefont|{      int tm_min;}|
   #, @schemefont|{      int tm_hour;}|
   #, @schemefont|{      int tm_mday;}|
   #, @schemefont|{      int tm_mon;}|
   #, @schemefont|{      int tm_year;}|
   #, @schemefont|{      int tm_wday;}|
   #, @schemefont|{      int tm_yday;}|
   #, @schemefont|{      int tm_isdst;}|
   #, @schemefont|{  };}|
   #, @schemeparenfont|{}}|))]

@section[#:tag "compilation"]{Compilation}

@defproc[(compile-header [header header?] [compiler ((listof query?) -> (listof uint))]) abi?]{
Compiles @scheme[header] using the given @scheme[compiler], producing an ABI. See @secref{abi}.}

A header compiler must recognize the following types of queries.

@; XXX: nail down the representation of @scheme[type].

@defproc[(query? [x any]) boolean?]{Determines whether @scheme[x] is a query}
@defstruct[query:sizeof ([type any])]{A query to determine the size of the type represented by @scheme[type].}
@defstruct[query:offset ([type any] [field symbol?])]{A query to determine the offset of @scheme[field] in @scheme[type].}
@defstruct[query:expr ([expr any])]{A query to determine the value of the integer expression @scheme[expr].}

@defproc[(system-compiler [#:include<> include<> (listof string?) '()] [#:include include (listof string?) '()] [exe ((-> any) -> any) gcc])
         ((listof query?) -> (listof uint))]{
A header compiler that delegates to an external C compiler, presumably installed on the
current system. The queries are converted into a series of C statements which are assembled
into a C program that produces the answers. The @scheme[include<>] list is used to generate
the list of system C headers to be included in the generated C program, and the @scheme[include]
list is the list of path strings for local headers included in the generated C program.
The @scheme[gcc] compiler is used by default, but this can be overridden by providing an
alternative system compiler for the @scheme[exe] argument.}

@section[#:tag "layout"]{Layouts}

@defproc[(layout? [x any]) boolean?]{}
@defproc[(primitive-layout? [x layout?]) boolean?]{}
@defproc[(ref-layout? [x layout?]) boolean?]{}
@defproc[(struct-layout? [x layout?]) boolean?]{}
@defproc[(union-layout? [x layout?]) boolean?]{}
@defproc[(array-layout? [x layout?]) boolean?]{}
@defproc[(pointer-layout? [x layout?]) boolean?]{}
@defproc[(enum-layout? [x layout?]) boolean?]{}
@defproc[(layout-size [x layout?]) uint]{}
@defproc[(layout-offset [x (or/c struct-layout? union-layout? enum-layout?)] [path (or/c symbol? (listof symbol?))]) uint]{}
@defproc[(struct-layout-lookup [x struct-layout?] [name symbol?]) layout?]{}
@defproc[(union-layout-lookup [x union-layout?] [name symbol?]) layout?]{}
@defproc[(deref-layout [x layout?]) layout?]{}

@section[#:tag "abi"]{Application Binary Interfaces (ABI's)}

An Application Binary Interface (ABI) provides information about the binary representation
of C datatypes on a particular architecture.

@defstruct[abi ([typedefs (hasheqof symbol? layout?)] [tags (hasheqof symbol? layout?)])]{
An ABI. The @scheme[typedefs] are a table of type definitions and the @scheme[tags] are a table
of @schemefont{struct}, @schemefont{union}, and @schemefont{enum} tag definitions.

As a convenience, ABI structures can be used as procedures, which is equivalent to calling
@scheme[abi-lookup] with the ABI structure as the first argument.}

@defproc[(abi-lookup [abi abi?] [name symbol?]) layout?]{Looks up the definition of @scheme[name] in @scheme[abi], searching
the type definitions first, and failing that, the tag definitions.}
@defproc[(abi-lookup-typedef [abi abi?] [name symbol?]) layout?]{Looks up the type definition of @scheme[name] in @scheme[abi].}
@defproc[(abi-lookup-tag [abi abi?] [name symbol?]) layout?]{Looks up the type tag definition of @scheme[name] in @scheme[abi].}
@defproc[(serialize-abi [abi abi?]) sexp?]{Serializes @scheme[abi] as an S-expression.}
@defproc[(deserialize-abi [input sexp?]) abi?]{Deserializes @scheme[input] as a representation of an ABI.}
@defproc[(read-abi [in input-port? (current-input-port)]) abi?]{Reads a serialized ABI from @scheme[in].}
@defproc[(write-abi [abi abi?] [out output-port? (current-output-port)]) any]{Serializes @scheme[abi] and writes it to @scheme[out].}
