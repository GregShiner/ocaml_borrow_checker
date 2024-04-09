open Sexplib

(* (* Read file 'test.sexp' and print it to stdout *)
   let () =
     let sexp = Sexp.load_sexp "test.sexp" in
     Sexp.output_hum stdout sexp *)

module Exp = struct
  type t =
    | Num of int
    | Id of string
    | Plus of { lhs : t; rhs : t }
    | Mult of { lhs : t; rhs : t }
    | Let of { symbol : string; rhs : t; body : t }
    | Lambda of { symbol : string; body : t }
    | App of { func : t; arg : t }
    | If of { cond : t; lhs : t; rhs : t }
    | Eq of { lhs : t; rhs : t }
    | Begin of t list
    | Bool of bool
    | Ref of t
    | MutRef of t
    | Box of t
    | Deref of t
    | Set of { lhs : t; rhs : t }

  let rec pp ppf this =
    match this with
    | Num n -> Format.fprintf ppf "Num(%i)" n
    | Id s -> Format.fprintf ppf "Id(%s)" s
    | Plus p -> Format.fprintf ppf "Plus(%a, %a)" pp p.lhs pp p.rhs
    | Mult m -> Format.fprintf ppf "Mult(%a, %a)" pp m.lhs pp m.rhs
    | Let l -> Format.fprintf ppf "Let(%s, %a, %a)" l.symbol pp l.rhs pp l.body
    | Lambda l -> Format.fprintf ppf "Lambda(%s, %a)" l.symbol pp l.body
    | App a -> Format.fprintf ppf "App(%a, %a)" pp a.func pp a.arg
    | If i -> Format.fprintf ppf "If(%a, %a, %a)" pp i.cond pp i.lhs pp i.rhs
    | Eq e -> Format.fprintf ppf "Eq(%a, %a)" pp e.lhs pp e.rhs
    | Begin b -> Format.fprintf ppf "Begin(%a)" (Format.pp_print_list pp) b
    | Bool b -> Format.fprintf ppf "Bool(%b)" b
    | Ref r -> Format.fprintf ppf "Ref(%a)" pp r
    | MutRef r -> Format.fprintf ppf "MutRef(%a)" pp r
    | Box b -> Format.fprintf ppf "Box(%a)" pp b
    | Deref d -> Format.fprintf ppf "Deref(%a)" pp d
    | Set s -> Format.fprintf ppf "Set(%a, %a)" pp s.lhs pp s.rhs
end

type location = int

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

  (* and binding = Binding of { name : string; value : t } *)
  (* and env = binding list *)
  and env = (string, t) Hashtbl.t

  let rec pp ppf this =
    match this with
    | Num n -> Format.fprintf ppf "Num(%i)" n
    | Closure c ->
        Format.fprintf ppf "Closure(%s, %a, %a)" c.arg Exp.pp c.body pp_env
          c.env
    | Bool b -> Format.fprintf ppf "Bool(%b)" b
    | Box l -> Format.fprintf ppf "Box(%i)" l
    | Ref r -> Format.fprintf ppf "Ref(%a)" pp r
    | MutRef r -> Format.fprintf ppf "MutRef(%a)" pp r
    | Moved -> Format.fprintf ppf "Moved"

  and pp_env ppf env =
    Format.printf "{";
    Hashtbl.iter (fun x y -> Format.fprintf ppf "%s -> %a" x pp y) env;
    Format.printf "}"

  (* and pp_binding ppf this = *)
  (*   match this with *)
  (*   | Binding b -> Format.fprintf ppf "Binding(%s, %a)" b.name pp b.value *)
end

type storage = (int, Value.t) Hashtbl.t

let (store : storage) = Hashtbl.create 100
let (mt_env : Value.env) = Hashtbl.create 100

let extend_env sym value env =
  Hashtbl.replace env sym value;
  env

let runtime_failure msg value =
  failwith (msg ^ Format.asprintf "%a" Value.pp value)

let handle_result (value : Value.t) (r : ('a, string) result) : 'a =
  match r with Ok v -> v | Error e -> runtime_failure e value

let rec parse = function
  | Sexp.Atom "true" -> Exp.Bool true
  | Sexp.Atom "false" -> Exp.Bool false
  | Sexp.Atom s -> ( try Exp.Num (int_of_string s) with Failure _ -> Exp.Id s)
  | Sexp.List
      [ Sexp.Atom "let"; Sexp.List [ Sexp.List [ Sexp.Atom id; e1 ] ]; e2 ] ->
      Exp.App
        { func = Exp.Lambda { symbol = id; body = parse e2 }; arg = parse e1 }
      (* Exp.Let { symbol = id; rhs = parse e1; body = parse e2 } *)
  | Sexp.List [ Sexp.Atom "+"; e1; e2 ] ->
      Exp.Plus { lhs = parse e1; rhs = parse e2 }
  | Sexp.List [ Sexp.Atom "*"; e1; e2 ] ->
      Exp.Mult { lhs = parse e1; rhs = parse e2 }
  | Sexp.List [ Sexp.Atom "lambda"; Sexp.List [ Sexp.Atom id ]; e ] ->
      Exp.Lambda { symbol = id; body = parse e }
  | Sexp.List [ Sexp.Atom "if"; e1; e2; e3 ] ->
      Exp.If { cond = parse e1; lhs = parse e2; rhs = parse e3 }
  | Sexp.List [ Sexp.Atom "="; e1; e2 ] ->
      Exp.Eq { lhs = parse e1; rhs = parse e2 }
  | Sexp.List (Sexp.Atom "begin" :: es) -> Exp.Begin (List.map parse es)
  | Sexp.List [ Sexp.Atom "&"; e ] -> Exp.Ref (parse e)
  | Sexp.List [ Sexp.Atom "!"; e ] -> Exp.MutRef (parse e)
  | Sexp.List [ Sexp.Atom "box"; e ] -> Exp.Box (parse e)
  | Sexp.List [ Sexp.Atom "@"; e ] -> Exp.Deref (parse e)
  | Sexp.List [ Sexp.Atom ":="; e1; e2 ] ->
      Exp.Set { lhs = parse e1; rhs = parse e2 }
  | Sexp.List [ e1; e2 ] -> Exp.App { func = parse e1; arg = parse e2 }
  | sexp -> failwith ("Invalid sexp: " ^ Sexp.to_string sexp)

let extractNum (l : Value.t) (r : Value.t) : int * int =
  match l with
  | Value.Num left -> (
      match r with
      | Value.Num right -> (left, right)
      | err -> runtime_failure "Right hand side is not a number " err)
  | err -> runtime_failure "Left hand side is not a number " err

let numPlus (left : Value.t) (right : Value.t) : Value.t =
  let l, r = extractNum left right in
  Value.Num (l + r)

let numMult (left : Value.t) (right : Value.t) : Value.t =
  let l, r = extractNum left right in
  Value.Num (l * r)

let rec lookup (sym : string) (env : Value.env) =
  match Hashtbl.find_opt env sym with
  | Some value -> (
      match value with
      | Moved -> failwith ("Usage of Moved Value: " ^ sym)
      | rest -> rest)
  | None -> failwith ("free variable: " ^ sym)

let get_nex_loc (store : storage) =
  (* Find the next available location in the store with a linear search *)
  let rec find_next_loc loc =
    if Hashtbl.mem store loc then find_next_loc (loc + 1) else loc
  in
  find_next_loc 0

let unbox (v : Value.t) : (Value.t, string) result =
  match v with
  | Value.Box loc -> (
      match Hashtbl.find_opt store loc with
      | Some v -> Ok v
      | None -> Error "PANIC: Unboxing a non-existent box: ")
  | _ -> Error "Not a box when unboxing: "

let set (lhs : Value.t) (rhs : Value.t) : (Value.t, string) result =
  match lhs with
  | Value.MutRef r -> (
      match r with
      | Value.Box loc -> (
          match Hashtbl.find_opt store loc with
          | Some _ ->
              Hashtbl.replace store loc rhs;
              Ok rhs
          | None -> Error "PANIC: Setting a non-existent box: ")
      | _ -> Error "PANIC: MutRef is not of a box: ")
  | _ -> Error "Cannot set a non-mutable reference: "

let move_symbol (sym : string) (oldsym : Exp.t) (value : Value.t)
    (envFrom : Value.env) (envTo : Value.env) =
  (match oldsym with Exp.Id i -> Hashtbl.replace envFrom i Moved | _ -> ());
  Hashtbl.replace envTo sym value;
  envTo

let rec interp (exp : Exp.t) (env : Value.env) : Value.t =
  match exp with
  | Exp.Num n -> Value.Num n
  | Exp.Id i -> lookup i env
  | Exp.Plus p ->
      let l = interp p.lhs env in
      let r = interp p.rhs env in
      numPlus l r
  | Exp.Mult m ->
      let l = interp m.lhs env in
      let r = interp m.rhs env in
      numMult l r
  | Exp.Let l ->
      let value = interp l.rhs env in
      interp l.body (move_symbol l.symbol l.rhs value env env)
  | Exp.Lambda l ->
      Value.Closure { arg = l.symbol; body = l.body; env = Hashtbl.copy env }
  | Exp.App a -> (
      let func = interp a.func env in
      let arg_val = interp a.arg env in
      match func with
      | Value.Closure c ->
          let value =
            interp c.body (move_symbol c.arg a.arg arg_val env c.env)
          in
          (match lookup c.arg c.env with
          | Value.Box b -> Hashtbl.remove store b
          | _ -> ());
          value
      | _ -> failwith "Not a function")
  | Exp.Bool b -> Value.Bool b
  | Exp.If i -> (
      let cond = interp i.cond env in
      match cond with
      | Value.Bool true -> interp i.lhs env
      | Value.Bool false -> interp i.rhs env
      | _ -> failwith "Not a boolean")
  | Exp.Eq e ->
      let l = interp e.lhs env in
      let r = interp e.rhs env in
      let ln, rn = extractNum l r in
      Value.Bool (ln = rn)
  | Exp.Begin b -> List.fold_left (fun _ e -> interp e env) (Value.Num 0) b
  | Exp.Box b ->
      let v = interp b env in
      let loc = get_nex_loc store in
      Hashtbl.add store loc v;
      Value.Box loc
  | Exp.Ref r -> (
      let v = interp r env in
      match v with
      | Value.Box _ -> Value.Ref v
      | rest -> runtime_failure "Not a box: " rest)
  | Exp.MutRef r -> (
      let v = interp r env in
      match v with
      | Value.Box _ -> Value.MutRef v
      | rest -> runtime_failure "Not a box: " rest)
  | Exp.Deref d -> (
      let v = interp d env in
      match v with
      | Value.Ref r | Value.MutRef r -> unbox r |> handle_result v
      | rest -> runtime_failure "Not a reference: " rest)
  | Exp.Set s ->
      let lhs = interp s.lhs env in
      let rhs = interp s.rhs env in
      set lhs rhs |> handle_result lhs

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
