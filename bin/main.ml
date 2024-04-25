open Sexplib
open Ocaml_borrow_checker

let parse_file filename =
  let sexp = Sexp.load_sexp filename in
  Parse.parse sexp

(* let parse_string str = *)
(*   let sexp = Sexp.of_string str in *)
(*   Parse.parse sexp *)

(* let eval sexp = *)
(*   let sexp = Sexp.of_string sexp in *)
(*   Format.printf "Output: %a\n%!" Interp.Value.pp *)
(*     (Interp.interp (Parse.parse sexp) Interp.mt_env) *)

let () =
  let filename = Sys.argv.(1) in
  let exp = parse_file filename in
  let _ = Analyze.analyze exp Analyze.mt_env in
  print_endline "All good!";
  Format.printf "Output: %a\n%!" Interp.Value.pp
    (Interp.interp exp Interp.mt_env)

let () = assert (Hashtbl.length Interp.store = 1)
