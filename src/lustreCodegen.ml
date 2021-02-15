open MiniCic.Constr
open MiniCic.Names
open MiniCic.Env
open MiniCic.Context.Named.Declaration
open MiniCic.Prod

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
  | "mult" -> "*"
  | "div" -> "div"
  | "mod" -> "mod"
  | _ -> assert false

let generate_expr c =
  let dependency = ref Id.Set.empty in
  let rec fetch_inner c idx =
    match c with
    | App (op, [| _; _; inner |]) when is_const_fst op ->
      idx, inner
    | App (op, [| _; _; inner |]) when is_const_snd op ->
      fetch_inner inner (idx + 1)
    | _ -> idx, c
  in
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
  let idx, inner = fetch_inner c 0 in
  let expr = aux inner in
  idx, expr, !dependency

let rec map_select_inner f c =
  match c with
  | App (op, [|t1; t2; inner|]) when is_const_fst op || is_const_snd op ->
    let inner' = map_select_inner f inner in
    if inner' == inner then
      c
    else
      App (op, [| t1; t2; inner' |])
  | _ -> f c

module ConstrMap = Map.Make (MiniCic.Constr)
module IntSet = Set.Make (MiniCic.Int)

(* Add more vars for uncatched multi-return. *)
let add_temp_var_for_multi_ret env =
  let env' = ref env in
  let name_suffix = ref 0 in
  let multi_ret_map = ref ConstrMap.empty in
  let rec gen_name () =
    let name = "r" ^ string_of_int !name_suffix in
    name_suffix := !name_suffix + 1;
    if (Id.Map.mem name !env'.env_named_context)
    then
      gen_name ()
    else
      name
  in
  (* Sort the vars. *)
  let id_body_list = Id.Map.fold (fun _ v statements ->
      match v with
      | LocalDef (id, body, t) ->
        let idx, _, _ = generate_expr body in
        (id, idx, t, body) :: statements
      | _ -> statements
    ) env.env_named_context []
  in
  (* Check the uncatched multi-return for var body. *)
  List.iter (fun (id, _, t, body) ->
      let rec aux c =
        if is_select c then
          let c = map_select_inner (MiniCic.Constr.map aux) c in
          if ConstrMap.mem c !multi_ret_map
          then
            mkVar (ConstrMap.find c !multi_ret_map)
          else
            let id = gen_name () in
            multi_ret_map := ConstrMap.add c id !multi_ret_map;
            let _, t, _ = parse_select c in
            env' := push_named (LocalDef (id, t, c)) !env';
            mkVar id
        else
          MiniCic.Constr.map aux c
      in
      let body' = map_select_inner aux body in
      if body == body' then
        ()
      else
        env' := push_named (LocalDef (id, t, body)) !env';
  ) id_body_list;
  (* For other leaked returns. *)
  let tuple_map = Id.Map.fold (fun _ v m ->
      match v with
      | LocalDef (_, body, _) when is_select body ->
        let idx, _, inner = parse_select body in
        ConstrMap.update inner (fun idx_map_opt ->
          match idx_map_opt with
          | None -> Some (IntSet.singleton idx)
          | Some idx_map -> Some (IntSet.add idx idx_map)
        ) m
      | _ -> m
  ) !env'.env_named_context ConstrMap.empty
  in
  ConstrMap.iter (fun tuple_body idx_set ->
    let tl = type_list_of_tuple_body !env' tuple_body in
    let _ = List.fold_left (fun idx t ->
      if IntSet.mem idx idx_set then
        idx + 1
      else
        let body = mk_select_with_type_list tl idx tuple_body in
        let id = gen_name () in
        env' := push_named (LocalDef (id, t, body)) !env';
        idx + 1
    ) 0 tl in
    ()) tuple_map;
  !env'

let generate_statement env params =
  let open! Json.Encode in
  let env = add_temp_var_for_multi_ret env in
  let params_set = Id.Set.of_list (List.map (fun x -> Id.of_string x.name) params) in
  let statements = Id.Map.fold (fun _ v statements ->
    match v with
    | LocalDef (id, body, _) ->
      let idx, expr, dependency = generate_expr body in
      let dependency = Id.Set.filter (fun e -> not (Id.Set.mem e params_set)) dependency in
      (id, idx, Json.stringify expr, dependency, 1) :: statements
    | _ -> statements
  ) env.env_named_context []
  in
  let statements = ref (Array.of_list statements) in
  let rec topo_sort idx =
    if Array.length !statements <> idx
    then (
      (* pending used to skip selected elements *)
      Array.stable_sort (fun (_,i1,  _, d1, pending1) (_, i2, _, d2, pending2) ->
        let diff = pending1 - pending2 in
        if diff <> 0 then diff
        else (
          let diff = Id.Set.cardinal d1 - Id.Set.cardinal d2 in
          if diff = 0 then i1 - i2 else diff
        )
      ) !statements;
      let cur_id, _, _, d, _ = !statements.(idx) in
      if (Id.Set.cardinal d) <> 0
      then (
        Js.log "circel detected";
        Js.log idx;
        Js.log !statements;
        assert false
      ) else (
        statements := Array.map (fun (id, i, e, d, pend) ->
          let pend = if pend = 0 || Id.equal id cur_id then 0 else 1 in
         id, i, e, (Id.Set.remove cur_id d), pend) !statements;
        topo_sort (idx + 1)
      )
    ) else ()
  in
  topo_sort 0;
  (* merge multiple-return statements *)
  let merged_statements = Array.fold_left (fun stats (id, idx, expr, _, _) ->
    if idx = 0
    then
      ([ id ], expr) :: stats
    else
      List.map (fun (ids, expr') ->
        if String.compare expr expr' = 0 then (id :: ids, expr) else (ids, expr')
      ) stats
    ) [] !statements
  in
  let merged_statements = List.map (fun (ids, expr) -> List.rev ids, expr) merged_statements in
  let merged_statements = List.rev merged_statements in
  (* merge done *)
  list (fun (ids, expr) ->
    match Json.parse expr with
    | Some expr ->
      object_ [
                ("op", string "assign");
                ("left", list (fun id -> string (Id.to_string id)) ids);
                ("right", expr)
          ]
    | _ -> assert false
    ) merged_statements

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