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

## The Library:
There are 3 components to the library: The parser, static analyzer, and interpreter.

### The Parser (lib/parse.ml)

The parser takes in a string and using the Sexp library the strings are first parsed into `Sexp.Atom`s and `Sexp.List`s which are variants of the type: `Sexp.t`. <!-- TODO: Examples -->
It then takes the tree of `Sexp.t`s and matches it to the various expressions in the grammar. This creates an `Exp.t` which is a type that contains the different variants of expressions.

> [!TIP]
> Note the use of `t` as the name of a type in OCaml modules. This is a common convention when a module (notably here, Sexplib's `Sexp` module, and our `Exp` module) has a type that is core to its functionality. You will see this convention used in other areas of the code.

### The Analyzer

The analyzer take in an expression tree and do a walk through of it, analyzing the code for errors such as usage of Moved values, doing it's job as the borrow checker, and type checking.

### The Interpreter

The interpreter take in an expression tree and walk through it while executing the code.
