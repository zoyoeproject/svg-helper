open MiniCic.Constr
open MiniCic.Names
open MiniCic.Env
open MiniCic.Context.Named.Declaration

type declare_unit = {
  name: string;
  typ: string;
}

let encode_declare_unit unit =
  let open! Json.Encode in (
      object_ [
        ("name", string unit.name);
        ("type", string unit.typ)
      ]
    )

type node_declare = {
  params: declare_unit list;
  returns: declare_unit list;
  vars: declare_unit list;
}

let encode_node_declare dec =
  let open! Json.Encode in (
      object_ [
        ("params", list encode_declare_unit  dec.params);
        ("returns", list encode_declare_unit dec.returns);
        ("vars", list encode_declare_unit dec.vars)
      ]
    )

let unit_to_string c =
  match c with
  | Const (n, _) -> Constant.label n |> Label.to_string
  | Var id -> Id.to_string id
  | Int i -> string_of_int i
  | Ind ((n, _), _) -> MutInd.label n |> Label.to_string
  | _ -> Js.log c; assert false

let generate_node_declare env =
  let params, vars, returns =
    Id.Map.fold (fun _ v (params, vars, returns) ->
    match v with
    | LocalAssum (id, t) ->
      Js.log id;
      let dec = {
        name = Id.to_string id;
        typ = unit_to_string t;
      } in
      dec :: params, vars, returns
    | LocalDef (id, _, t) ->
      Js.log id;
      let dec = {
        name = Id.to_string id;
        typ = unit_to_string t;
      } in
      if MiniCic.Env.is_exported id env then
        params, vars, dec :: returns
      else
        params, dec :: vars, returns
  ) env.env_named_context ([], [], [])
  in
  {
    params = params;
    returns = returns;
    vars = vars
  }

let is_case_info_bool ci =
  let (n, _) = ci.ci_ind in
  (MutInd.to_string n) = "core.bool"

let to_notation name =
  match name with
  | "plus" -> "+"
  | "minus" -> "-"
  | "eq" -> "="
  | "neq" -> "<>"
  | "lt" -> "<"
  | "gt" -> ">"
  | "not" -> "not"
  | _ -> assert false

let generate_expr c =
  let dependency = ref Id.Set.empty in
  let rec aux c =
    let open! Json.Encode in
    match c with
    | App (op, args) -> (
      try
        object_ [
          ("notation", string (to_notation (unit_to_string op)));
          ("args", array aux args)
        ]
      with _ ->
        object_ [
          ("func", string (unit_to_string op));
          ("args", array aux args)
        ]
    )
    | Case (ci, _, cond, dispatches) when is_case_info_bool ci ->
      object_ [
        ("notation", string "if");
        ("args", list aux [ cond; dispatches.(0); dispatches.(1) ])
      ]
    | Var id ->
      dependency := Id.Set.add id !dependency;
      string (unit_to_string c)
    | _ -> string (unit_to_string c)
  in
  let expr = aux c in
  expr, !dependency

let generate_statement env params =
  let open! Json.Encode in
  let params_set = Id.Set.of_list (List.map (fun x -> Id.of_string x.name) params) in
  let statements = Id.Map.fold (fun _ v statements ->
    match v with
    | LocalDef (id, body, _) ->
      let expr, dependency = generate_expr body in
      let dependency = Id.Set.filter (fun e -> not (Id.Set.mem e params_set)) dependency in
      (id, expr, dependency) :: statements
    | _ -> statements
  ) env.env_named_context []
  in
  let statements = ref (Array.of_list statements) in
  let rec topo_sort idx =
    if Array.length !statements <> idx
    then (
      Array.stable_sort (fun (_, _, d1) (_, _, d2) ->
        Id.Set.cardinal d1 - Id.Set.cardinal d2) !statements;
      let id, _, d = !statements.(idx) in
      if (Id.Set.cardinal d) <> 0
      then (
        Js.log "circel detected";
        Js.log idx;
        Js.log !statements;
        assert false
      ) else (
        statements := Array.map (fun (i, e, d) -> i, e, Id.Set.remove id d) !statements;
        topo_sort (idx + 1)
      )
    ) else ()
  in
  topo_sort 0;
  array (fun (id, expr, _) ->
      object_ [
                ("op", string "assign");
                ("left", string (Id.to_string id));
                ("right", expr)
          ]
    ) !statements

let codegen_ast_json env =
  let declare = generate_node_declare env in
  let statements = generate_statement env declare.params in
  let open! Json.Encode in
  let json =
    object_ [
      ("declares", encode_node_declare declare);
      ("statements", statements);
    ]
  in
  Js.log (Json.stringify json);
  json

let codegen_from_ctx env =
  let ctxt = Context.get_global_context () in
  let env = CfgEditor.generate_env_from_node_map ctxt.nodes env in
  codegen_ast_json env