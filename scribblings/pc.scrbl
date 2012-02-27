#lang scribble/doc

@require[scribble/manual
         scribble/eval
         scribble/basic
         planet/scribble
         "utils.rkt"]

@require[(for-label racket/base
                    racket/contract
                    (this-package-in header)
                    (this-package-in eval)
                    (this-package-in ast)
                    (this-package-in pc))]

@title[#:tag "pc"]{Parenthetical C}

@defmodule/this-package[pc]

For now, this library only provides a few convenience forms for constructing AST nodes.
Eventually I intend to create a ``Parenthetical C'' language with an S-Expression-based
alternative syntax for the whole of C.

@deftogether[[
@defform/subs[#:id typedef
              (typedef type alias-id)
              ([type struct type-id])]
@defform*/subs[#:id struct
               [(struct tag-id)
                (struct tag-id (struct-field ...))
                (struct (struct-field ...))]
               [(struct-field [type field-id] field-id)]]
@defform*/subs[#:id union
               [(union tag-id)
                (union tag-id (union-variant ...))
                (union (union-variant ...))]
               [(union-variant [type variant-id] variant-id)]]
@defform*/subs[#:id enum
               [(enum tag-id)
                (enum tag-id (enum-variant ...))
                (enum (enum-variant ...))]
               [(enum-variant [variant-id expr] variant-id)]]
]]

@defproc[(array [type type?] [size expr?]) type:array?]{}
@defproc[(pointer [type type?]) type:pointer?]{}
