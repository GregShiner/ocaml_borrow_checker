open Parse

type location = int

module AnalVal = struct
  type t =
    | Num
    | Closure of { arg : string; body : Exp.t; env : env_tree }
    | Bool
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
  and env_tree = { trunk : env_tree option; branch : env }

  let rec pp ppf this =
    match this with
    | Num -> Format.fprintf ppf "Num"
    | Closure c ->
        Format.fprintf ppf "Closure(%s, %a, %a)" c.arg Exp.pp c.body pp_env
          c.env
    | Bool -> Format.fprintf ppf "Bool"
    | Box l -> Format.fprintf ppf "Box(%i)" l
    | Ref r -> Format.fprintf ppf "Ref(%a)" pp r
    | MutRef r -> Format.fprintf ppf "MutRef(%a)" pp r
    | Moved -> Format.fprintf ppf "Moved"

  and pp_env ppf env_tree =
    Format.printf "{";
    Hashtbl.iter
      (fun x y -> Format.fprintf ppf "%s -> %a" x pp y)
      env_tree.branch;
    Format.printf "}"

  (* and pp_binding ppf this = *)
  (*   match this with *)
  (*   | Binding b -> Format.fprintf ppf "Binding(%s, %a)" b.name pp b.value *)
end

type storage = (int, AnalVal.t) Hashtbl.t

let (store : storage) = Hashtbl.create 100
let (mt_env : AnalVal.env_tree) = { trunk = None; branch = Hashtbl.create 10 }

let extend_env sym value env =
  Hashtbl.replace env sym value;
  env

let runtime_failure msg value =
  failwith (msg ^ Format.asprintf "%a" AnalVal.pp value)

let handle_result (value : AnalVal.t) (r : ('a, string) result) : 'a =
  match r with Ok v -> v | Error e -> runtime_failure e value

let rec lookup (sym : string) (env : AnalVal.env_tree) =
  match Hashtbl.find_opt env.branch sym with
  | Some value -> (
      match value with
      | Moved -> Error ("Usage of Moved Value: " ^ sym)
      | rest -> Ok rest)
  | None -> (
      match env.trunk with
      | Some trunk -> lookup sym trunk
      | None -> failwith ("Free variable: " ^ sym))

let get_nex_loc (store : storage) =
  (* Find the next available location in the store with a linear search *)
  let rec find_next_loc loc =
    if Hashtbl.mem store loc then find_next_loc (loc + 1) else loc
  in
  find_next_loc 0

let unbox (v : AnalVal.t) : (AnalVal.t, string) result =
  match v with
  | AnalVal.Box b -> (
      match Hashtbl.find_opt store b with
      | Some v -> Ok v
      | None -> Error "PANIC: Unboxing a non-existent box: ")
  | _ -> Error "Not a box when unboxing: "

let set (lhs : AnalVal.t) (rhs : AnalVal.t) : (AnalVal.t, string) result =
  match lhs with
  | AnalVal.MutRef r -> (
      match r with
      | AnalVal.Box b -> (
          match Hashtbl.find_opt store b with
          (* Check that the types of the two values are the same *)
          | Some prev_value ->
              if prev_value = rhs then Ok rhs
              else
                failwith
                  (Format.asprintf "Cannot set type %a to type %a" AnalVal.pp
                     prev_value AnalVal.pp rhs)
          | None -> Error "PANIC: Setting a non-existent box: ")
      | _ -> Error "PANIC: MutRef is not of a box: ")
  | _ -> Error "Cannot set a non-mutable reference: "

let rec lookup_box_in_trunk (sym : string) (env : AnalVal.env_tree) =
  match Hashtbl.find_opt env.branch sym with
  | Some (AnalVal.Box _) -> Some env.branch
  | _ -> (
      match env.trunk with
      | Some trunk -> lookup_box_in_trunk sym trunk
      | None -> None)

let move_symbol (sym : string) (oldsym : Exp.t) (value : AnalVal.t)
    (env : AnalVal.env_tree) =
  (match oldsym with
  | Exp.Id i -> (
      match lookup_box_in_trunk i env with
      | Some e ->
          Hashtbl.replace e i Moved;
          Hashtbl.replace env.branch i Moved
      | _ -> ())
  | _ -> ());
  Hashtbl.replace env.branch sym value;
  env

let rec check_borrow (loc : location) (isMutable : bool)
    (env : AnalVal.env_tree) =
  let check _ (value : AnalVal.t) =
    match isMutable with
    | true -> (
        match value with
        | AnalVal.MutRef (AnalVal.Box l) when l = loc ->
            failwith "can not create multiple mutable reference"
        | AnalVal.Ref (AnalVal.Box l) when l = loc ->
            failwith
              "can not create mutable reference while immutable reference exist"
        | _ -> ())
    | false -> (
        match value with
        | AnalVal.MutRef (AnalVal.Box l) when l = loc ->
            failwith
              "can not create immutable reference while mutable reference exist"
        | _ -> ())
  in
  Hashtbl.iter check env.branch;
  match env.trunk with Some e -> check_borrow loc isMutable e | _ -> ()

let rec analyze (exp : Exp.t) (env : AnalVal.env_tree) : AnalVal.t =
  match exp with
  | Exp.Num _ -> AnalVal.Num
  | Exp.Id i -> ( match lookup i env with Ok v -> v | Error e -> failwith e)
  | Exp.Plus p ->
      let _ = analyze p.lhs env in
      let _ = analyze p.rhs env in
      AnalVal.Num
  | Exp.Mult m ->
      let _ = analyze m.lhs env in
      let _ = analyze m.rhs env in
      AnalVal.Num
  | Exp.Let l ->
      let value = analyze l.rhs env in
      let return_val = analyze l.body (move_symbol l.symbol l.rhs value env) in
      (match lookup l.symbol env with
      | Ok (AnalVal.Box b) -> Hashtbl.remove store b
      | _ -> ());
      return_val
  | Exp.Lambda l ->
      AnalVal.Closure
        {
          arg = l.symbol;
          body = l.body;
          env = { trunk = Some env; branch = Hashtbl.create 10 };
        }
  | Exp.App a -> (
      let func = analyze a.func env in
      let arg_val = analyze a.arg env in
      match func with
      | AnalVal.Closure c ->
          let value = analyze c.body (move_symbol c.arg a.arg arg_val c.env) in
          (match lookup c.arg c.env with
          | Ok (AnalVal.Box b) -> (
              match value with
              | AnalVal.Box vb when b != vb -> Hashtbl.remove store b
              | _ -> ())
          | _ -> ());
          value
      | _ -> failwith "Not a function")
  | Exp.Bool _ -> AnalVal.Bool
  | Exp.If i -> (
      let cond = analyze i.cond env in
      match cond with
      | AnalVal.Bool ->
          let l = analyze i.lhs env in
          let r = analyze i.rhs env in
          if l = r then l else failwith "Branches of if must have the same type"
      | _ -> failwith "Not a boolean")
  | Exp.Eq e ->
      let l = analyze e.lhs env in
      let r = analyze e.rhs env in
      if l = r then
        match l with
        | AnalVal.Num -> AnalVal.Bool
        | AnalVal.Bool -> AnalVal.Bool
        | _ -> failwith "Equality only defined for numbers and booleans"
      else failwith "The two sides of the equality must be the same type"
  | Exp.Begin b -> List.fold_left (fun _ e -> analyze e env) AnalVal.Num b
  | Exp.Box b ->
      let v = analyze b env in
      let loc = get_nex_loc store in
      Hashtbl.add store loc v;
      AnalVal.Box loc
  | Exp.Unbox u -> (
      let v = analyze u env in
      match v with
      | AnalVal.Box loc -> (
          match Hashtbl.find_opt store loc with
          | Some v -> v
          | None -> failwith "PANIC: Unboxing a non-existent box: ")
      | _ -> failwith "Not a box when unboxing")
  | Exp.Ref r -> (
      let v = analyze r env in
      match v with
      | AnalVal.Box l ->
          check_borrow l false env;
          AnalVal.Ref v
      | rest -> runtime_failure "Not a Box: " rest)
  | Exp.MutRef r -> (
      let v = analyze r env in
      match v with
      | AnalVal.Box l ->
          check_borrow l true env;
          AnalVal.MutRef v
      | rest -> runtime_failure "Not a Box: " rest)
  | Exp.Deref d -> (
      let v = analyze d env in
      match v with
      | AnalVal.Ref r | AnalVal.MutRef r -> unbox r |> handle_result v
      | rest -> runtime_failure "Not a reference: " rest)
  | Exp.Set s ->
      let lhs = analyze s.lhs env in
      let rhs = analyze s.rhs env in
      set lhs rhs |> handle_result lhs
  | Exp.Display d ->
      let v = analyze d env in
      (* Format.printf "%a\n" AnalVal.pp v; *)
      v
