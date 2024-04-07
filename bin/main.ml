open Sexplib;;
(* (* Read file 'test.sexp' and print it to stdout *)
let () =
  let sexp = Sexp.load_sexp "test.sexp" in
  Sexp.output_hum stdout sexp *)

module Exp = struct
    type t = 
        | Num of int
        | Id of string
        | Plus of {lhs: t; rhs: t}
        | Mult of {lhs: t; rhs: t} 
        | Let of {symbol: string; rhs: t; body: t}
        | Lambda of {symbol: string; body: t}
        | App of {func: t; arg: t}
        | If of {cond: t; lhs: t; rhs: t}
        | Eq of {lhs: t; rhs: t}
        | Begin of t list
    let rec pp ppf this = match this with
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
end

module Value = struct
    type t = 
        | Num of int 
        | Closure of {arg: string; body: Exp.t; env: env}
    and binding = 
        | Binding of {name: string; value: t}
    and env = binding list

    let rec pp ppf this = match this with
        | Num n -> Format.fprintf ppf "Num(%i)" n
        | Closure c -> Format.fprintf ppf "Closure(%s, %a, %a)" c.arg Exp.pp c.body pp_env c.env
    and pp_env ppf env = Format.fprintf ppf "[%a]" (Format.pp_print_list pp_binding) env
    and pp_binding ppf this = match this with
        | Binding b -> Format.fprintf ppf "Binding(%s, %a)" b.name pp b.value
end

let mt_env = [];;
let extend_env l r = l :: r  (* same as cons *)

module Storage = struct
    type t = 
        | Cell of {location: int; value: Value.t}
end;;

type store = Storage.t list;;

let mt_store = [];;
let override_store l r = l :: r;;  (* same as cons *)

type return = {value: Value.t; store: Storage.t list};;

let rec parse = function
    | Sexp.Atom s -> 
        (try Exp.Num (int_of_string s)
        with Failure _ -> Exp.Id s)
    | Sexp.List [Sexp.Atom "let"; Sexp.List [Sexp.List [Sexp.Atom id; e1]]; e2] -> 
        Exp.Let {symbol = id; rhs = parse e1; body = parse e2}
    | Sexp.List [Sexp.Atom "+"; e1; e2] -> 
        Exp.Plus {lhs = parse e1; rhs = parse e2}
    | Sexp.List [Sexp.Atom "*"; e1; e2] -> 
        Exp.Mult {lhs = parse e1; rhs = parse e2}
    | Sexp.List [Sexp.Atom "lambda"; Sexp.List [Sexp.Atom id]; e] -> 
        Exp.Lambda {symbol = id; body = parse e}
    | Sexp.List [Sexp.Atom "if"; e1; e2; e3] -> 
        Exp.If {cond = parse e1; lhs = parse e2; rhs = parse e3}
    | Sexp.List [Sexp.Atom "="; e1; e2] -> 
        Exp.Eq {lhs = parse e1; rhs = parse e2}
    | Sexp.List (Sexp.Atom "begin" :: es) -> 
        Exp.Begin (List.map parse es)
    | Sexp.List [e1; e2] ->
        Exp.App {func = parse e1; arg = parse e2}
    | _ -> failwith "parse error"

let extractNum ( l : Value.t ) ( r : Value.t ): int * int = 
    match l with
        | Closure _ -> failwith "Left hand side is not a number"
        | Value.Num left -> match r with 
                            | Value.Num right -> (left, right)
                            | Value.Closure _ -> failwith "Right hand side is not a number"

let numPlus (left : Value.t) (right: Value.t) : Value.t = 
    let (l, r) = extractNum left right in
        Value.Num(l + r)

let numMult (left : Value.t) (right: Value.t) : Value.t = 
    let (l, r) = extractNum left right in
        Value.Num(l * r)

let rec lookup ( sym : string ) ( env : Value.env ) = 
    match env with 
    | Binding fst :: rst -> if fst.name = sym then fst.value else lookup sym rst
    | _ -> failwith ("free variable: " ^ sym)

let rec interp (exp: Exp.t) (env: Value.env) (sto: store) : return =
    match exp with
    | Exp.Num n -> {value = Value.Num n; store = sto}
    | Exp.Id i -> {value = lookup i env; store = sto}
    | Exp.Plus p -> let l = interp p.lhs env sto in
                        let r = interp p.rhs env l.store in
                            {value = numPlus l.value r.value; store = r.store}
    | Exp.Mult m -> let l = interp m.lhs env sto in
                        let r = interp m.rhs env l.store in
                            {value = numMult l.value r.value; store = r.store}
    | Exp.Let l -> let rhs = interp l.rhs env sto in 
                        interp  l.body 
                                (extend_env 
                                    (Value.Binding{name = l.symbol; value = rhs.value})
                                    env) 
                                rhs.store
    | Exp.Lambda l -> {value = Value.Closure{arg = l.symbol; body = l.body; env = env}; store = sto}
    | Exp.App a -> let func = interp a.func env sto in
                        let arg = interp a.arg env func.store in
                            (match func.value with
                                | Value.Closure c -> interp c.body 
                                                        (extend_env 
                                                            (Value.Binding{name = c.arg; value = arg.value})
                                                            c.env) 
                                                        arg.store
                                | _ -> failwith "Not a function")
    | _ -> failwith "Not Implemented"

let parse_file filename = 
    let sexp = Sexp.load_sexp filename in
    parse sexp

let eval sexp = 
    let sexp = Sexp.of_string sexp in
        Format.printf "Output: %a\n%!" Value.pp (interp (parse sexp) mt_env mt_store).value

let () = eval "(let ((x 1)) (+ x 1))"
