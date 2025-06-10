# Development Setup
First, install OCaml by following the guide [here](https://ocaml.org/install).
The rest of this guide will assume you have activated, at least the global, opam switch. This should mean that you have access to `dune` commands.
If you cannot use dune, try running the following command, and maybe put it in your `.bashrc` or some other script called when an interactive shell starts.
```sh
eval $(opam env)
```
This step should not be necessary, if you answered "yes" to when `opam init` asked if you would like it to put the OCaml tools in your PATH.
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
This will emit a binary file at `_build/default/bin/main.exe` (Yes, it does emit a file with the `.exe` extension even on Linux, despite it being a normal Linux executable)
This executable does not rely on any files so it can be safely copied to anywhere you would like. Execute a program with the following command:
```sh
main.exe <input file>
```

## Running with Dune
If you would like to instead build and immediately execute the program with , you can use the following command:
```sh
dune exec ocaml_borrow_checker -- <input file>
```

# Outline

The code are separated into two parts:
- The main file (bin/main.ml)
- The library (lib/)

## Main file

The main file only contains some helper functions that tie the whole program together by reading the provided file and feeding it into the parser, then the expression tree to the analyzer, then finally it runs the interpreter on the expression tree.

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

### The Analyzer (lib/analyze.ml)

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
module Value = struct
  type t =
  | Num of int
  | Bool of bool
...
end
```

However, the other data types such as `Closure`s, `Box`s, and `Ref`s are unchanged. The idea of keeping the interpreter and analyzer similar stems from the fact that they both rely on the same mechanism of recursive descent.

### The Interpreter (lib/interp.ml)

The interpreter take in an expression tree and walks through it while executing the program. 
It has some pretty basic checks for some errors that are mostly there to ensure all match arms are handled. 
It relies on the guarantees that the static analyzer makes, pretty heavily. It does not check for things like invalid borrows or some type checking conditions.
However, theoretically none of the errors should be reachable if they pass the static analyzer. This is, of course, assuming no bugs exist in the analyzer. 

The interpreter functions by doing a recursive descent on the expression tree and passing back up `Value.t`s which contain the return values of functions. Following the previous example, interpreting `(+ (* 5 3) 2)` will produce, `Value.Num 17`.

# Language
This program was written in OCaml and uses Jane Street's `Sexplib` [library](https://github.com/janestreet/sexplib) for s-exp handling.
The language is a heavily modified version of the Curly language from Matt Flatt's Utah CS 3520 course found [here](https://my.eng.utah.edu/~cs3520/f19/). The memory management system was inspired by Rust's memory model. It implements borrows and moves in a similar fashion to Rust. 
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
> At the time of writing, this check does not always work and will be fixed soon.

For example, the following code is invalid:
```lisp
(let ((x (box 5)))      ; Allocate x on the heap
    (let ((ref (& x)))  ; Create a reference to x
        (let ((y x))    ; Move x into y (x is no longer valid)
            (@ ref))))  ; Dereference the reference to x; ERROR: Cannot dereference a reference to a moved value
```

# Interpretation and Analysis

## Store and Environment

The store is a hash table with the key being an integer which functions as a pseudo location in memory, which allows for mutations of declared variables.

The environment is also a hash table which map variables, a.k.a symbols, to its declared value.

The values in both the store and environment are `Value.t`s. (during analysis, `AnalVal.t`s are used. See Outline -> Main file -> The Analyzer.)
```ocaml
module Value = struct
  type t =
    | Num of int
    | Closure of { arg : string; body : Exp.t; env : env }
    | Bool of bool
    | Box of location
      (* Box represents a value on the heap NOT a reference (analogous to a rust owned value *)
    | Ref of t
      (* Ref represents an immutable borrow of a value on the heap (can only be created for boxes) *)
    | MutRef of t
      (* MutRef represents a mutable borrow of a value on the heap (can only be created for boxes) *)
    | Moved
```

## Interpreter

The interpreter starts at the top of the syntax tree and recursively interprets all of the expressions throughout the tree.

### Exp.Num

`Value.Num` is returned.

### Exp.Bool

`Value.Bool` is returned.

### Exp.Id

The `lookup` helper function is called which gets the value of the symbol from the environment and returns it. An error will be thrown if it is a `Moved` value, or if it doesn't exist.

### Exp.Plus

Interpret the left and right hand side of the expression and add them together.

### Exp.Mult

Interpret the left and right hand side of the expression and multiply them together.

### Exp.Eq

Interpret the left and right hand side of the expression and return true if they are the same, false otherwise.

### Exp.Lambda

Return `Value.Closure` with the environment inside the closure being a copy of the current environment, which allows for variable shadowing.

### Exp.App

There is three parts to this operation, which occur at different stages of the expression, with the following syntax:

```lisp
(Func Arg)
```

The first is interpreting the `Func` which is being applied. If `Func` does not return a `Closure` then a "Not a function" error is thrown.

Then `Arg` is interpreting, it is the value mapped to the symbol inside the Closure. The mapping of the value inside the environment of the closure is done via a helper function `move_symbol`. It checks if `Arg` is a symbol, and if its value is a  `Value.Box` inside the current environment. If it is, it will change that symbol to a `Moved` value. Then the `Arg` is mapped to the symbol inside the environment of `Closure`.

Finally, the body of the `Func` is interpreted with the new binding, and before its value is returned, a clean up operation will occur. This will check whether the return value is a `Value.Box` and if it is the same `Value.Box` as the `Arg`. If it is not then it removes the value inside store.

### Exp.If

Interpret the condition and if it is true then return the interpreted value of left hand-side, otherwise return the interpreted value of right hand-side.

### Exp.Begin

Fold left is used to interpret the list of expressions, with the same environment, which allow for mutation of the environment to be shared across all interpret calls within the `Begin`.

Mutation of environment is needed for checking when a value is moved and to prevent its usage.

### Exp.Box

Return a `Value.Box` with the location in store that is mapped to the interpreted value. To determine the location to store the value at, it will do a linear search across the store to find the next available location in the store by integer.

### Exp.Unbox

Return the value inside store that `Value.Box` pointed to, throw error if there's nothing at the location.

### Exp.Ref

Return `Value.Ref`, which serves as a copy of `Value.Box`, allowing it to be moved and use at multiple places without moving the actual box. It also follows the borrow checking rules described above.

### Exp.MutRef

Return `Value.MutRef`, which serves as a copy of `Value.Box`, allow for mutation the box location in store. It also follows the borrow checking rules described above.

### Exp.Deref

Return `Value.t` of the value at a `Value.Ref` or `Value.MutRef` location in the store.

### Exp.Set

Mutate value at `Value.MutRef` location, and return the new value.

### Display

Print value to console, return value printed.

### Debug

Print debug format of value and return value.

## Analysis

Most of the static analysis steps are repeats of the interpret step without storing the actual value or computation, so only the differences will be discussed. The primary purpose of the static analyzer is to check code for semantic errors. If a static analyzer can check for many different kinds of errors upfront, then the interpreter can make more assumptions about the program which can make it even more performant. Ex: Our interpreter does not perform borrow checking. It simply assumes all borrows are valid because they passed the borrow checker in the static analyzer.

### Environment

In analysis, the environment is a tree structure instead of of a flat hash table, this allows for changes of earlier environments to be reflected in subsequence analysis calls deeper down the syntax tree and prevent usage of moved values. 

### Borrow checker

In analysis, an additional borrow checker step is added to ensure:
- Multiple mutable references can not exist.
- Mutable and immutable references can not co-exist.

The borrow checker is ran when the analyzer encounters a `Exp.Ref` or `Exp.MutRef`.

In the case of `Exp.Ref` the borrow checker iterates through the environment and checks if a mutable reference already exists. If it does, then throw an error.

For `Exp.MutRef` the borrow checker iterates through the environment and checks if an immutable or mutable reference already exists. If it does, then throw an error.

### Type checking

The static analysis step also checks the return type for the left hand side and right hand side to make sure they're the same/make sense for the following expressions:
- `Exp.If`
  - `cond` must resolve to a `Value.Bool`
- `Exp.Eq`
  - Both must be numbers (may be extended to booleans at a later date)
- `Exp.Plus`
  - Both must be numbers
- `Exp.Mult`
  - Both must be numbers

# Insight

## Strengths
- LISP based language, easy to extend features
- Ownerships, Fast memory clean up without the use of a garbage collector
- Safe mutability with the borrow checker
- Type safety, no surprises about what type a value is
- Analyzer happens at compiles time, so interpretation is faster as it can make assumptions about the safety of the program.

## Weakness
- Gets stuck in an infinite loop when the analyzer runs into a recursive function (to be fixed)
- Pretty small language with not many practical features

## Improvements
- Create a separate intermediate representation for analyzing (High Level IR) and interpreting (Low Level IR) code
  - Allow analysis of recursive functions
    - `let-rec` form will be treated as a special form during analysis (HIR), but will be desugared when converting to LIR for interpretation
  - Change interpreter to treat ref and mutref as the same thing as the analyzer already did a check, which increase speed.
  - HIR = Larger Grammar, better for analyzing
  - LIR = Smaller Grammar, faster for interpreting, less cases to check
- Trim down interpreter
  - Many unnecessary checks left over from previous iterations that are now handled by the analyzer
- Separate out the build steps for parsing/analyzing and interpreting
  - Compile once, run whenever

# Demos
All of the demos can be found in the `demos/` directory. There is also a `demo.sh` script that simply runs each demo in order. 
The following screenshots are some select demos being ran using the `demo.sh` script.
> [!NOTE]
> The `demo.sh` script optionally uses the [bat](https://crates.io/crates/bat) program to get syntax highlighted output. You can install bat if you have [Rust and Cargo installed](https://doc.rust-lang.org/cargo/getting-started/installation.html).
> Simply run `cargo install bat`
> If you don't have the bat command, the demo script will just use `cat` instead.
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/7fbea689-01f4-49cd-adb6-27a4876831be)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/a569fcb6-3b2c-4381-b439-5ed157262acc)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/ca406cac-6724-4287-8d6a-cc2ec62293ea)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/e2769438-91e7-4986-836b-25ec96be2ee6)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/8bd9b6f8-fe78-4396-85c7-0202664e7b7a)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/4d5c9a06-0724-420d-9477-73697c83cc38)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/e5aea931-ae79-412d-9435-7b688534cde5)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/856510bf-6126-422a-bade-e5e3414b06e2)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/561427b7-51f2-4de9-a22b-9a4553fb6f31)
![image](https://github.com/GregShiner/ocaml_borrow_checker/assets/3160083/a7621bcb-48c9-49ea-9af3-bb938cd57152)
