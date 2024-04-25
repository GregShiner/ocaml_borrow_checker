open Sexplib

module Exp = struct
  type t =
    | Num of int
    | Id of string
    | Plus of { lhs : t; rhs : t }
    | Mult of { lhs : t; rhs : t }
    | Lambda of { symbol : string; body : t }
    | App of { func : t; arg : t }
    | If of { cond : t; lhs : t; rhs : t }
    | Eq of { lhs : t; rhs : t }
    | Let of { symbol : string; rhs : t; body : t }
    | Begin of t list
    | Bool of bool
    | Ref of t
    | MutRef of t
    | Box of t
    | Unbox of t
    | Deref of t
    | Set of { lhs : t; rhs : t }
    | Display of t

  let rec pp ppf this =
    match this with
    | Num n -> Format.fprintf ppf "Num(%i)" n
    | Id s -> Format.fprintf ppf "Id(%s)" s
    | Plus p -> Format.fprintf ppf "Plus(%a, %a)" pp p.lhs pp p.rhs
    | Mult m -> Format.fprintf ppf "Mult(%a, %a)" pp m.lhs pp m.rhs
    | Lambda l -> Format.fprintf ppf "Lambda(%s, %a)" l.symbol pp l.body
    | App a -> Format.fprintf ppf "App(%a, %a)" pp a.func pp a.arg
    | If i -> Format.fprintf ppf "If(%a, %a, %a)" pp i.cond pp i.lhs pp i.rhs
    | Eq e -> Format.fprintf ppf "Eq(%a, %a)" pp e.lhs pp e.rhs
    | Begin b -> Format.fprintf ppf "Begin(%a)" (Format.pp_print_list pp) b
    | Bool b -> Format.fprintf ppf "Bool(%b)" b
    | Ref r -> Format.fprintf ppf "Ref(%a)" pp r
    | MutRef r -> Format.fprintf ppf "MutRef(%a)" pp r
    | Box b -> Format.fprintf ppf "Box(%a)" pp b
    | Unbox u -> Format.fprintf ppf "Unbox(%a)" pp u
    | Let l -> Format.fprintf ppf "Let(%s, %a, %a)" l.symbol pp l.rhs pp l.body
    | Deref d -> Format.fprintf ppf "Deref(%a)" pp d
    | Set s -> Format.fprintf ppf "Set(%a, %a)" pp s.lhs pp s.rhs
    | Display d -> Format.fprintf ppf "Display(%a)" pp d
end

(* (define mk-rec-fun
   `{lambda {body-proc}
      {let {[fX {lambda {fX}
                  {let {[f {lambda {x}
                             {{fX fX} x}}]}
                    {body-proc f}}}]}
        {fX fX}}}) *)
let mk_rec_fun =
  "(lambda (body-proc)\n\
  \             (let ((fX (box (lambda (fX)\n\
  \                         (let ((F (box (lambda (x)\n\
  \                                    ((fX fX) x)))))\n\
  \                             (body-proc (@ (& F))))))))\n\
  \                 ((@ (& fX)) (@ (& fX)))))"

let rec parse = function
  | Sexp.Atom "true" -> Exp.Bool true
  | Sexp.Atom "false" -> Exp.Bool false
  | Sexp.Atom s -> ( try Exp.Num (int_of_string s) with Failure _ -> Exp.Id s)
  | Sexp.List
      [ Sexp.Atom "let"; Sexp.List [ Sexp.List [ Sexp.Atom id; e1 ] ]; e2 ] ->
      Exp.App
        { func = Exp.Lambda { symbol = id; body = parse e2 }; arg = parse e1 }
  | Sexp.List
      (* [ *)
      (*   Sexp.Atom "let-begin"; Sexp.List [ Sexp.List [ Sexp.Atom id; e1 ] ]; e2; *)
      (* ] -> *)
      (Sexp.Atom "let-begin"
      :: Sexp.List [ Sexp.List [ Sexp.Atom id; e1 ] ]
      :: e2) ->
      Exp.Let
        { symbol = id; rhs = parse e1; body = Exp.Begin (List.map parse e2) }
  | Sexp.List
      [ Sexp.Atom "let-rec"; Sexp.List [ Sexp.List [ Sexp.Atom id; e1 ] ]; e2 ]
    ->
      parse
        (Sexp.of_string
           (Printf.sprintf "(let ((%s (%s (lambda (%s) %s)))) %s)" id mk_rec_fun
              id (Sexp.to_string e1) (Sexp.to_string e2)))
  (* | Sexp.List *)
  (*     [ Sexp.Atom "let-rec"; Sexp.List [ Sexp.List [ Sexp.Atom id; e1 ] ]; e2 ] *)
  (*   -> *)
  (*     parse *)
  (*       (Sexp.of_string *)
  (*          (Printf.sprintf "(let ((%s (%s (lambda (%s) %s)))) %s)" id mk_rec_fun *)
  (*             id (Sexp.to_string e1) (Sexp.to_string e2))) *)
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
  | Sexp.List [ Sexp.Atom "unbox"; e ] -> Exp.Unbox (parse e)
  | Sexp.List [ Sexp.Atom "@"; e ] -> Exp.Deref (parse e)
  | Sexp.List [ Sexp.Atom ":="; e1; e2 ] ->
      Exp.Set { lhs = parse e1; rhs = parse e2 }
  | Sexp.List [ Sexp.Atom "display"; e ] -> Exp.Display (parse e)
  | Sexp.List [ e1; e2 ] -> Exp.App { func = parse e1; arg = parse e2 }
  | sexp -> failwith ("Invalid sexp: " ^ Sexp.to_string sexp)
