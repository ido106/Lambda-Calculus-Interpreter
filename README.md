[To the previous PL exercise - NOS and SOS Semantics](https://github.com/ido106/NOS-SOS-Semantics) | [To the next PL exercise - Prolog](https://github.com/ido106/Prolog)
# Lambda Calculus Interpreter
In this exercise, we will build a Parser and Interpeter for [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) in the [Ocaml](https://ocaml.org/) language.  
In the `reducer.ml` file, we will implement a parser for calculating Lambda Expressions. The functions are:  
`fv: term-> StringSet.t`
The function recives a `term` and returns the list of the free variables in it.  
The set of free variables can be defined inductively as follows:  

    FV(x) = {x}
    FV(𝜆𝑥. 𝑡) = FV(t) – {x}
    FV(𝑡1 𝑡2) = FV(𝑡1) ∪ FV(𝑡2)
  The next function is  
  

    substitute: string -> term -> term -> term
This function applies substitution including [alpha-conversion](https://sookocheff.com/post/fp/alpha-conversion/) if necessary.  

The next function is  

    reduce_cbv: term -> term option
This function implements one calculation step (reduction) according to **call-by-value** semantics. We will assume that the only values are abstractions.  
Similarly, the function  

    reduce_cbn : term -> term option

Applies one step of calculation (reduction) according to **call-by-name** semantics.  

### Running Instructions
The solution should compile with OCaml 4.02, and does not require Core.  
Your files should compile with:  

    $ ocamlc -o tests utils.ml lexer.ml parser.ml reducer.ml tests.ml

And then run like this:  

    $ ./tests

After you compile, you can also use [utop](https://opam.ocaml.org/blog/about-utop/) to interactively test your implementations like this:  

    $ utop -I .
    utop # #load_rec "tests.cmo";;
    utop # open Parser;;
    utop # open Reducer;;
    utop # open Tests;;
    utop # evaluate ~verbose:true reduce_normal (parse "((\\x. x) y)");;
