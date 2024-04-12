open Sexplib

let parse_file filename =
  let sexp = Sexp.load_sexp filename in
  parse sexp

let parse_string str =
  let sexp = Sexp.of_string str in
  parse sexp

let eval sexp =
  let sexp = Sexp.of_string sexp in
  Format.printf "Output: %a\n%!" Value.pp (interp (parse sexp) mt_env)

(* let () = eval "(let ((x 1)) (+ x 1))" *)
let () =
  Format.printf "Output: %a\n%!" Value.pp
    (interp (parse_file "test.sexp") mt_env)

let () =
  Format.printf "Store: \n";
  Format.printf "{\n";
  Hashtbl.iter (fun x y -> Format.printf "%i -> %a\n" x Value.pp y) store;
  Format.printf "}\n"
