#lang scribble/doc

@require[scribble/manual
         scribble/eval
         scribble/basic
         planet/scribble
         "abnf.rkt"
         "utils.rkt"]

@require[(for-label racket/base
                    (only-in scribblings/foreign/unsafe-foreign ptr-ref ptr-set!)
                    (this-package-in main))]

@title[#:tag "intro"]{C Metaprogramming Utilities}

This package provides utilities for manipulating C programs.

@section[#:tag "started"]{Getting Started}

The easiest way to get started using the C metaprogramming utilities is with the main module:

@defmodule/this-package[]

This module provides everything in the entire package. Subsequent sections of this
manual describe the functionality of the individual libraries included, which can also be
required individually.

@;NOTE: this is completely faked--see the hack in utils.ss.
@examples[#:eval the-eval
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
                         [int tm_isdst]))))
          (define time
            (compile-header time.h
                            (system-compiler #:include<> '("time.h") gcc)))
          (layout-offset (time 'tm) 'tm_year)]

Binary layout information is especially useful for interacting with the foreign
library---see @other-manual['(lib "scribblings/foreign/foreign.scrbl")]. For example,
the procedures @scheme[ptr-ref] and @scheme[ptr-set!] can be used to read and write
from arbitrary addresses, which can be computed using layout information.

@section[#:tag "libraries"]{Libraries Provided by this Package}

This package includes:

@itemize[
  @item{A library of abstract syntax for the C99 language--see @secref["syntax"]}
  @item{A library for parsing C99 programs--see @secref["parsing"]}
  @item{A (preliminary) library providing S-expression syntax for C--see @secref["pc"]}
  @item{A library for compiling and running C programs with an external compiler--see @secref["eval"]}
  @item{A library for compiling C header information--see @secref["header"]}
]

@section[#:tag "limitations"]{Known Limitations}

The @seclink["parse"]{parser} does not recognize the C preprocessor. I may attempt to implement
the preprocessor in the future, but there's no guarantee at this point.

The @seclink["grammar"]{grammar} does not support any extensions for GCC, MSVC, or any other specific
C implementations. I intend to add support for these extensions on a by-need basis.
Specific requests (as well as patches) are welcome.

@seclink["eval"]{External compilation} is currently limited to GCC. I intend to add support for
additional compilers on a by-need basis;
patches are welcome.

@section[#:tag "feedback"]{Feedback and Bug Reports}

Before sending feedback or bug reports, please consult the
@link["http://planet.plt-scheme.org/trac/query?status=accepted&status=assigned&status=needinfo&status=new&status=reopened&component=dherman%2Fc.plt&order=priority&col=id&col=summary&col=status&col=type&col=priority&col=milestone&col=component"
      "current set of registered issues"].
If you cannot find your issue there, feel free to
@link["http://planet.plt-scheme.org/trac/newticket?component=dherman/c.plt&owner=dherman"]{file a new bug report}
in the online issue database.

Any other feedback may be emailed to me, Dave Herman, at @link["mailto:dherman@ccs.neu.edu" "dherman@ccs.neu.edu"].

@include-section["history.scrbl"]
