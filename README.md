# Development Setup
First, install OCaml by following the guide [here](https://ocaml.org/install).
The rest of this guide will assume you have activated, at least the global, opam switch. This should mean that you have access to `dune` commands.
If you cannot use dune, try running the following command, and maybe put it in your `.bashrc` or some other script called when an interactive shell starts.
```sh
eval $(opam env)
```
This step should be taken care of however, if you answered "yes" to when `opam init` asked if you would like it to put the OCaml tools in your PATH.
If however, this doesn't work, or you don't want to add the tools to your PATH, you can use dune and other tool commands with:
```sh
opam exec -- [CMD]
```

This package depends on Jane Street's `Sexplib` [library](https://github.com/janestreet/sexplib) for s-exp handling. To install it, run the following command:
```sh
opam install sexplib
```

# Usage
## Compiling
If you would like to use the application as a binary, run the following command to build the program into a single executable:
```sh
dune build
```
This will emit a binary file at `_build/default/bin/main.exe` (Yes, it does emit a file with the `.exe` extension even on linux, despite it being a normal linux executable)
This executable does not rely on any files so it can be safely copied to anywhere you would like. Execute a program with the following command:
```sh
main.exe <input file>
```

## Running with Dune
If you would like to instead build and immediately execute the program with , you can use the following command:
```sh
dune exec ocaml_borrow_checker -- <input file>
```

<!--
TODO: Output examples and explaination
-->

# Outline

The code are seperated into two parts:
- The main file (bin/main.ml)
- The library (lib/)

## Main file

The main file only contains some helper functions that tie the whole program together by reading the provided file and feeding it into the parser, then the expression tree to the analyzer, then finally it runs interp on the expression tree.

## The Library
There are 3 components to the library: the parser, static analyzer, and interpreter.

### The Parser (lib/parse.ml)

The parser takes in a string and using the Sexp library the strings are first parsed into `Sexp.Atom`s and `Sexp.List`s which are variants of the type: `Sexp.t`. Here is a quick example of how the s-exp, `(+ (* 5 3) 2)` would look as a structure of `Sexp.t`s:

```ocaml
Sexp.List [ Sexp.Atom "+"; Sexp.List [ Sexp.Atom "*"; Sexp.Atom "5"; Sexp.Atom "3"; ]; Sexp.Atom "2"; ]
```

It then takes the tree of `Sexp.t`s and matches it to the various expressions in the grammar. This creates an `Exp.t` which is a type that contains the different variants of expressions. This type is recursive and is used to represent the expression tree.

> [!TIP]
> Note the use of `t` as the name of a type in OCaml modules. This is a common convention when a module (notably here, Sexplib's `Sexp` module, and our `Exp` module) has a type that is core to its functionality. You will see this convention used in other areas of the code.

Here is an example of an `Exp.t` structure using the previous example:

```ocaml
Exp.Plus { lhs = Exp.Mult { lhs = Exp.Num 5; rhs = Exp.Num 3; }; rhs = Exp.Num 2; }
```

### The Analyzer

The analyzer takes in an expression tree and does a walk through it, analyzing the code for errors such as usage of moved values, borrow checking, and type checking. It works extremely similarly to the interpreter but works with `AnalVal.t`s instead of `Value.t`s. The primary difference between these 2 types is that it doesn't keep track of actual data, such as numbers and booleans.

Analyzer:
```ocaml
module AnalVal = struct
  type t =
  | Num
  | Bool
...
end
```

Interpreter:
```ocaml
module AnalVal = struct
  type t =
  | Num of int
  | Bool of bool
...
end
```

However, the other data types such as `Closure`s, `Box`s, and `Ref`s are unchanged. The idea of keeping the interpreter and analyzer similar stems from the fact that they both rely on the same mechanism of recursive descent.

### The Interpreter

The interpreter take in an expression tree and walks through it while executing the program. 
It has some pretty basic checks for some errors that are mostly there to ensure all match arms are handled. 
It relies on the guarantees that the static analyzer makes, pretty heavily. It does not check for things like invalid borrows or some type checking conditions.
However, theoretically none of the errors should be reachable if they pass the static analyzer. This is, of course, assuming no bugs exist in the analyzer. 

The interpreter functions by doing a recursive descent on the expression tree and passing back up `Value.t`s which contain the return values of functions. Following the previous example, interpreting `(+ (* 5 3) 2)` will produce, `Value.Num 17`.

# Grammar
This program was written in OCaml and uses Jane Street's `Sexplib` [library](https://github.com/janestreet/sexplib) for s-exp handling.
The language is a heavily modified version of the Curly language from Matt Flatt's Utah CS 3520 course found [here](https://my.eng.utah.edu/~cs3520/f19/). The memory mangement system was inspired by Rust's memory model. It implements borrows and moves in a similar fashion to Rust. 
## Full Expression Tree
The following is an excerpt from `lib/parse.ml`
```ocaml
module Exp = struct
  type t =
    | Num of int (* Number *)
    | Id of string (* Symbolic identifier *)
    | Plus of { lhs : t; rhs : t } (* Addition; lhs and rhs must resolve to a Num *)
    | Mult of { lhs : t; rhs : t } (* Multiplication; lhs and rhs must resolve to a Num *)
    | Lambda of { symbol : string; body : t } (* Lambda function *)
    | App of { func : t; arg : t } (* Application of a function *)
      (* Conditional; cond must resolve to a Bool; resolves to lhs when cond is true, otherwise rhs *)
    | If of { cond : t; lhs : t; rhs : t } (* lhs and rhs must resolve to the same type *)
    | Eq of { lhs : t; rhs : t } (* Equality; lhs and rhs must resolve to numbers *)
    | Begin of t list (* Sequence of expressions; resolves to the last expression *)
    | Bool of bool (* Boolean *)
    | Ref of t (* Immutable ref; can only be made on boxes; see borrow checking rules for more *)
    | MutRef of t (* Mutable ref; can only be made on boxes; see borrow checking rules for more *)
    | Box of t (* Boxed value; can be borrowed as a ref; represents a heap allocated value *)
    | Unbox of t (* Get the value stored in a box *)
    | Deref of t (* Get the value stored in a ref *)
    | Set of { lhs : t; rhs : t } (* Set the value stored in a mutable ref *)
    | Display of t (* If num or bool, prints value to stdout, otherwise, pretty prints the value *)
    | Debug of t (* Pretty prints the value to stdout *)
```
## Grammar
The grammar for the language is as follows:
```
<exp> ::= <num>
        | <id>
        | true
        | false
        | (+ <exp> <exp>)
        | (* <exp> <exp>)
        | (lambda (<id>) <exp>)
        | (<exp> <exp>)           ; function application
        | (if <exp> <exp> <exp>)
        | (= <exp> <exp>)
        | (begin <exp>*)
        | (& <exp>)               ; immutable reference
        | (! <exp>)               ; mutable reference
        | (box <exp>)
        | (unbox <exp>)
        | (@ <exp>)               ; dereference
        | (:= <exp> <exp>)        ; set mutable reference
        | (display <exp>)
        | (debug <exp>)
```
Adding the sugar expressions, there is also the following:
```
<exp> ::= (let ((<id> <exp>)) <exp>)
        | (let-begin ((<id> <exp>)) <exp>*)
        | (let-rec ((<id> <exp>)) <exp>)
```
In Addition to these base expressions, there are a couple of sugars to extend the language.
## Sugars
### Let
The `let` form is a syntactic sugar for a lambda function application. The sugar looks like the following:
```lisp
(let ((id rhs))
  body)
```
desugars to the following:
```lisp
((lambda (id) body) rhs)
```
This sugar is really powerful for our language. The idea is that we would only have to make all of our language features work with simple lambda function applications. This would then make other language features such as `let` work implicitly with things like borrow checking and move semantics.

### Let-begin
The `let-begin` form is simply a convenience sugar for the common pattern of having a `begin` expression as the body of a `let` expression. The following pattern:
```lisp
(let-begin ((id rhs))
  (expr1)
  (expr2)
  ...)
```
expands to
```lisp
(let ((id rhs))
  (begin
    (expr1)
    (expr2)
    ...))
```

### Recursive Bindings
> [!CAUTION]
> Recursive bindings are not yet implemented in the analyzer due to a technical limitation. Currently, they will send the analyzer into an infinite loop. This will be addressed at a later date.
> If you would *really* like to use them, you can do so by commenting out [line 20 in bin/main.ml](https://github.com/GregShiner/ocaml_borrow_checker/blob/main/bin/main.ml#L20).
> This is extremely untested, so use at your own risk.
