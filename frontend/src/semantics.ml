(*
type environment =
  | ESystemFunction
  | EFunction
  | ELabel
  | EModule of symbol_table
 and symbol_table = (string, environment) Hashtbl.t

type semantics = {
  mutable env: environment;
  mutable func_index: int;
}

let rec lookup s name =
  let find tbl =
    try
      Some (Hashtbl.find tbl name, tbl)
    with
      Not_found -> None
  in
  (*let find_with_retry parent_env tbl =
    let fe = find tbl in
    match fe with
      None -> lookup parent_env name
    | v -> v
  in*)
  match s.env with
    EModule sym_tbl -> find sym_tbl
  | _ -> None

type sem =
    SProgram of sem list
  | IlligalLeaf

let rec prescan env a = match a with
    Ast.Program statemants -> SProgram (List.map (prescan env) statemants)
  | Ast.ExprStatement expr -> prescan table expr
  | Ast.BinaryExpr(lhs, op, rhs) -> IlligalLeaf
  | Ast.UnaryExpr(op, rhs) -> IlligalLeaf
  | Ast.Id(name) -> lookup env
  | _ -> IlligalLeaf

let semantics_analysis a =
  let m: symbol_table = Hashtbl.create 10 in
  Hashtbl.add m "foo" (ESystemFunction);

  let table = {
    env = EModule(m);
    func_index = 0;
  } in

  ignore (prescan table a);
  ()
 *)
