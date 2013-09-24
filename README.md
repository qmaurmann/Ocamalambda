Ocamalambda
===========

Interpreter (with parser) for toy language "Little Lambda" defined in blog post ["What is a programming language?"](http://qmaurmann.wordpress.com/2013/08/25/what-is-a-programming-language-2/), written as a first exercise in OCaml.

Little Lambda is a small Lisp-like language; a program consists of a single expression, i.e. one of the forms

    <expression> ::= <integer>
                   | <symbol>
                   | (+ <expression> <expression>)
                   | ( * <expression> <expression>)
                   | (if0 <expression> <expression> <expression>)
                   | (lambda (<symbol>) <expression>)
                   | (<expression> <expression>)

Evaluate an expression by applying the function run to it (as a string).

The [corresponding Lisp code](https://gist.github.com/qmaurmann/6330497#file-littlelambda-rkt) weighs in under 20 lines, but the comparison isn't totally fair because parsing is totally free in Lisp (via the builtin quote), whereas parsing is actually the trickiest step in any other language.


TODO:

* Switch to bignums to prevent silent integer overflow

* Expand to the larger, impure language of the [Proglang](https://github.com/qmaurmann/Proglang) repository

* Add a REPL?
