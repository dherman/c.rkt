#lang scribble/doc

@require[scribble/manual
         scribble/basic]

@title[#:tag "top"]{@bold{C} Metaprogramming Utilities}

by @author+email["Dave Herman" "dherman@ccs.neu.edu"]

This package provides utilities for manipulating C programs, including a library
for extracting binary layout information from header files for use with the foreign
library---see @other-manual['(lib "scribblings/foreign/foreign.scrbl")].

For license information, please see the file @tt{COPYING.LIB}.

@table-of-contents[]

@include-section["intro.scrbl"]

@include-section["syntax.scrbl"]

@include-section["parse.scrbl"]

@include-section["pc.scrbl"]

@include-section["header.scrbl"]

@include-section["eval.scrbl"]

@include-section["internals.scrbl"]

@index-section[]
