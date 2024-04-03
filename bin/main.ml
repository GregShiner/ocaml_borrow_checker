open Sexplib;;
(* (* Read file 'test.sexp' and print it to stdout *)
let () =
  let sexp = Sexp.load_sexp "test.sexp" in
  Sexp.output_hum stdout sexp *)

module Exp = struct
    type t = 
        | Num of int
        | Id of string
        | Plus of t * t
        | Mult of t * t
        | Let of string * t * t
        | Lambda of string * t
        | App of t * t
        | If of t * t * t
        | Eq of t * t
        | Begin of t list
end

module Value = struct
    type t = 
        | Num of int 
        | Closure of string * Exp.t * env
    and binding = string * t
    and env = binding list
end

let rec parse = function
    | Sexp.Atom s -> 
        (try Exp.Num (int_of_string s)
        with Failure _ -> Exp.Id s)
    | Sexp.List [Sexp.Atom "let"; Sexp.Atom id; e1; e2] -> 
        Exp.Let (id, parse e1, parse e2)
    | Sexp.List [Sexp.Atom "+"; e1; e2] -> 
        Exp.Plus (parse e1, parse e2)
    | Sexp.List [Sexp.Atom "*"; e1; e2] -> 
        Exp.Mult (parse e1, parse e2)
    | Sexp.List [Sexp.Atom "lambda"; Sexp.List [Sexp.Atom id]; e] -> 
        Exp.Lambda (id, parse e)
    | Sexp.List [Sexp.Atom "if"; e1; e2; e3] -> 
        Exp.If (parse e1, parse e2, parse e3)
    | Sexp.List [Sexp.Atom "="; e1; e2] -> 
        Exp.Eq (parse e1, parse e2)
    | Sexp.List (Sexp.Atom "begin" :: es) -> 
        Exp.Begin (List.map parse es)
    | Sexp.List [e1; e2] ->
        Exp.App (parse e1, parse e2)
    | _ -> failwith "parse error"

let parse_file filename = 
    let sexp = Sexp.load_sexp filename in
    parse sexp
