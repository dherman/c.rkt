#lang scribble/doc

@require[scribble/manual
         scribble/basic
         "utils.rkt"]

@title[#:tag "history"]{History}

@itemize[
@hist-item[(version 0 1) 2009 2 1]{Initial release. Reasonably usable for header extraction.}
@hist-item[(version 0 2) 2009 3 9]{Full C99 parser implemented.}
@hist-item[(version 0 3) 2009 3 31]{Some cleanup of the AST API.}
@hist-item[(version 0 4) 2009 9 16]{Fixed @link["http://planet.plt-scheme.org/trac/ticket/202"]{parser bug 202}: the grammar should be complete now.}
@hist-item[(version 0 5) 2012 2 26]{Updated for Racket.}
]
