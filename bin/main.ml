open Sexplib
open Ocaml_borrow_checker

let parse_file filename =
  let sexp = Sexp.load_sexp filename in
  Parse.parse sexp

let parse_string str =
  let sexp = Sexp.of_string str in
  Parse.parse sexp

let eval sexp =
  let sexp = Sexp.of_string sexp in
  Format.printf "Output: %a\n%!" Interp.Value.pp
    (Interp.interp (Parse.parse sexp) Interp.mt_env)

(* let () = eval "(let ((x 1)) (+ x 1))" *)
let () =
  Format.printf "Output: %a\n%!" Analyze.AnalVal.pp
    (Analyze.analyze (parse_file "test.sexp") Analyze.mt_env);
  Format.printf "Output: %a\n%!" Interp.Value.pp
    (Interp.interp (parse_file "test.sexp") Interp.mt_env)

let () = assert (Hashtbl.length Interp.store = 0)
