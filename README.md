# Development Setup
This package depends on Jane Street's `Sexplib` library for s-exp handling. To install it, run the following command:
```sh
opam install sexplib
```

<!-- To use utop with `core`, add the following to your `.ocamlinit` file:
```ocaml
#use "topfind";;
#thread;;
#camlp4o;;
(* #require "core.top";; *)
(* #require "core.syntax";; *)
#require "core";;
open Core.std;;
``` -->


