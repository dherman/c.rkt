#lang scribble/doc

@require[scribble/manual
         scribble/eval
         scribble/basic
         planet/scribble
         "utils.rkt"]

@require[(for-label racket/base
                    racket/contract
                    (this-package-in eval))]

@title[#:tag "eval"]{Evaluation}

This library provides utilities for building and running C programs with
a system-installed C compiler. Currently the only supported compiler is
@link["http://gcc.gnu.org"]{GCC}.

@defmodule/this-package[eval]

@section[#:tag "processes"]{External Processes}

@defstruct[(exn:fail:process exn:fail) ([out input-port?] [error-out input-port?])]{
Raised when a system error occurs in executing an external process.}

@section[#:tag "gcc"]{GCC}

@defparam[current-gcc gcc (or/c path? #f)]{The path to the GCC executable.
By default, this is initialized to:
@schemeblock[
(or (find-executable-path "gcc")
    (find-executable-path "gcc.exe"))
]}

@defproc[(gcc [print-source (-> any)]) (values input-port? input-port?)]{
Runs a C program with GCC, using the value of @scheme[current-gcc] to find the GCC executable.
The program source is obtained by invoking @scheme[print-source], which is run in a continuation
barrier to prevent re-entrant evaluation. The external program is built to a temporary file which
is automatically deleted after the program terminates.

The external program is run in a separate thread, so this procedure returns before the program
necessarily terminates. The procedure produces two values, input ports for reading from the external
program's @schemefont{stdout} and @schemefont{stderr} ports.}
