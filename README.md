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

# Language
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
> The sugar is implemented, but is extremely untested, so use at your own risk.

The `let-rec` form provides a mechanism for creating recursive functions. It has the exact same form as `let`, except it allows the use of the symbol defined within the rhs of the binding. Here is an example:
```lisp
(let-rec ((mult
  (lambda (x)
    (lambda (y)
      (if (= x 0)
          0
          (+ y ((mult (+ x -1)) y)))))))
  (let ((m (mult 5)))
    (m 6)))
```
Here, `mult` is a recursive function that implements multiplication.

## Move Semantics
> [!NOTE]
> Move semantics and borrow checking rules only apply to values in the store (boxed values)

> [!NOTE]
> Because `let` is implemented as a sugar for a lambda function application, move semantics work exactly the same for both, because they are treated as equivalent.

The following is `demos/3-1let_moves.larry` found [here](https://github.com/GregShiner/ocaml_borrow_checker/blob/main/demos/3-1let_moves.larry)
```lisp
#|
    Demo 5: Moves within a Let
        - Values are moved when they are reasigned within a let
|#

(let ((x (box 5)))  ; Allocates a box in the store           (store: x -> 5)
    (let ((y x))    ; The value stored at x is moved into y  (store: x -> Moved, y -> 5)
        (unbox y))) ; Getting value out of y
```
This program outputs 5.
Here, first, the value 5, is pointed to by x. This means that x "owns" the value 5. Then, the ownership of 5, is moved from x to y. This means that the identifier, x, is no longer valid, and the value can be accessed from y instead.

Here is the same example, but with an error.
```lisp
#|
    Demo 6: ERROR Moves within a Let
        - ERROR you cannot use a value after move
        - Using a value in 2 places would require a copy
            or multiple pointers to the same value
            which is unsafe
|#

(let ((x (box 5)))  ; Allocates a box in the store          (store: x -> 5)
    (let ((y x))    ; The value stored at x is moved into y (store: x -> Moved, y -> 5)
        (unbox x))) ; Getting value out of x, ERROR: x has been moved
```
This program produces the following error: `Usage of Moved Value: x`
x cannot be accessed at the end because it's value has been moved into y.

Note that in terms of implementation, this means that the value bound to a symbol that gets moved is changed to a type called `Value.Moved`. This allows the analyzer to easily check for use of Moved value later in the program.

## Borrowing and Borrow Checking
Borrowing provides a mechanism for shared memory access. There are 2 types of borrows:
- Immutable: read-only
- Mutable: read-write

In order for these references to be valid, they must pass a set of rules defined by a system known as the borrow checker.
All programs must follow these 2 rules:
- At any given time, you can have either one mutable reference or any number of immutable references.
- References must always be valid.
Notice that these are the exact same rules as defined [here](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html#the-rules-of-references) in the rust book.

### Reference creation restrictions
Let's break down what these 2 rules mean. The first one means, in our language specifically, you can either have
- One mutable reference
- Any number of immutable references
bound on the stack at any given time. This means that you can have multiple mutable references to any given value, so long as they do not ever exist in the same scope.

There actually is one notable difference here between this language and Rust. Rust defines that the scope of a reference is between when it is created, and the last time it is used, NOT the end of its lexical scope. 

Take the following example from [References and Borrows](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html#mutable-references) in the rust book:
```rust
fn main() {
    let mut s = String::from("hello");

    let r1 = &s; // no problem
    let r2 = &s; // no problem
    println!("{} and {}", r1, r2);
    // variables r1 and r2 will not be used after this point

    let r3 = &mut s; // no problem
    println!("{}", r3);
}
```
is a completely valid program. However, in this language, an equivalent program will not pass the analyzer.
```lisp
(let ((x (box 5)))
    (let ((ref1 (& x)))            ; Create an immutable reference to x
        (let-begin ((ref2 (& x)))  ; Create another immutable reference to x
            (display (@ ref1))     ; Display the value stored at ref1
            (display (@ ref2))     ; Display the value stored at ref1
            (let ((ref3 (! x)))    ; Create a mutable reference to x; ERROR: Cannot create a mutable reference while an immutable reference exists
                (display (@ ref3))))))
```
This program will not pass the analyzer because ref3 (a mutable reference) is created in the same scope as ref1 and ref2 (immutable references). This is despite the fact that ref1 and ref2 are never used after ref3 is created.

### Reference Validity
The next rule is a bit more vague, "References must always be valid." In this language, this simply means that you cannot dereference a reference to a moved value. Practically, this is actually consistent with Rust's definition of reference scoping (a reference's scope ends after it is last used). But in our language, under the hood, a reference to a moved value *can* exist, however, it cannot be used, so it doesn't really matter, since this error will always be caught by the analyzer.
> [!CAUTION]
> At the time of writting, this check does not always work and will be fixed soon.

For example, the following code is invalid:
```lisp
(let ((x (box 5)))      ; Allocate x on the heap
    (let ((ref (& x)))  ; Create a reference to x
        (let ((y x))    ; Move x into y (x is no longer valid)
            (@ ref))))  ; Dereference the reference to x; ERROR: Cannot dereference a reference to a moved value
```
