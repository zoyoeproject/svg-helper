open MiniCic.Constr
open MiniCic.Names
open MiniCic.Env
open MiniCic.Context.Named.Declaration
open MiniCic.Prod

type declare_unit = {name: string; typ: string}

let encode_declare_unit unit =
  let open! Json.Encode in
  object_ [("name", string unit.name); ("type", string unit.typ)]

type node_declare =
  { statics: declare_unit list
  ; params: declare_unit list
  ; returns: declare_unit list
  ; vars: declare_unit list }

let encode_node_declare dec =
  let open! Json.Encode in
  object_
    [ ("statics", list encode_declare_unit dec.statics)
    ; ("params", list encode_declare_unit dec.params)
    ; ("returns", list encode_declare_unit dec.returns)
    ; ("vars", list encode_declare_unit dec.vars) ]

let rec unit_to_string c =
  match c with
  | Const (n, _) -> Constant.label n |> Label.to_string
  | Var id -> Id.to_string id
  | Int i -> string_of_int i
  | Ind ((n, _), _) -> MutInd.label n |> Label.to_string
  | Prod _ -> prod_to_string c
  | _ ->
      Js.log (MiniCic.Pp.to_string c) ;
      assert false
  and prod_to_string typ =
    let _params, c = Aux.collect_type_params typ in
    let _params = Array.fold_left (fun acc (n, t) ->
      let sep = if acc = "" then "" else "; " in
      acc ^ sep ^ n ^ ":" ^ (unit_to_string t)
      ) "" _params
    in
    let params = "(" ^ _params ^ ")" in
    let _outputs = Aux.collect_prod_outputs c in
    let _outputs = Array.fold_left (fun acc (n, t) ->
      let sep = if acc = "" then "" else "; " in
      acc ^ sep ^ (Aux.get_id_of_name n) ^ ":" ^ (unit_to_string t)
      ) "" _outputs
    in
    let outputs = "(" ^ _outputs ^ ")" in
    params ^ " returns " ^ outputs

let sort_static env statics =
  let open MiniCic.Constr in
  Array.of_list statics
  |> Array.map (fun dec ->
         ( dec
         , let rec aux deps c =
             match c with
             | Var id -> Id.Set.add id deps
             | _ -> MiniCic.Constr.fold aux deps c
           in
           match MiniCic.Env.lookup_named dec.name env with
           | LocalAssum (_, t) -> aux Id.Set.empty t
           | _ -> assert false ) )
  |> TopoSort.topo_sort (fun dec -> Id.Set.remove dec.name) Id.Set.cardinal
  |> Array.to_list

let generate_node_declare env =
  let statics, params, vars, returns =
    Id.Map.fold
      (fun _ v (statics, params, vars, returns) ->
        match v with
        | LocalAssum (id, t) ->
            Js.log id ;
            let dec = {name= Id.to_string id; typ= unit_to_string t} in
            if Id.Set.mem id env.env_static then
              (dec :: statics, params, vars, returns)
            else (statics, dec :: params, vars, returns)
        | LocalDef (id, _, t) ->
            Js.log id ;
            let dec = {name= Id.to_string id; typ= unit_to_string t} in
            if MiniCic.Env.is_exported id env then
              (statics, params, vars, dec :: returns)
            else (statics, params, dec :: vars, returns) )
      env.env_named_context ([], [], [], [])
  in
  let statics = sort_static env statics in
  {statics; params; returns; vars}

let is_case_info_bool ci =
  let n, _ = ci.ci_ind in
  MutInd.to_string n = "core.bool"

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

let generate_expr env c =
  let dependency = ref Id.Set.empty in
  let rec fetch_inner c idx =
    match c with
    | App (op, [|_; _; inner|]) when is_const_fst op -> (idx, inner)
    | App (op, [|_; _; inner|]) when is_const_snd op ->
        fetch_inner inner (idx + 1)
    | _ -> (idx, c)
  in
  let rec aux c =
    let open! Json.Encode in
              Js.log (MiniCic.Pp.to_string c);
    match c with
    | App (op, args) -> (
      try
        object_
          [ ("notation", string (to_notation (unit_to_string op)))
          ; ("args", array aux args) ]
      with _ ->
        let args = Array.map aux args in
        let args =
          match op with
          | Const (const, _) when (not (is_const_fst op)) && (not (is_const_snd op)) ->
              Js.log env;
              let info = MiniCic.Env.lookup_constant env const in
              Array.mapi
                (fun i arg ->
                  object_
                    [("explicit", bool info.arguments.(i)); ("value", arg)] )
                args
          | _ ->
              Array.mapi
                (fun _ arg -> object_ [("explicit", bool true); ("value", arg)])
                args
        in
        object_
          [ ("func", string (unit_to_string op))
          ; ("args", array (fun x -> x) args) ] )
    | Case (ci, _, cond, dispatches) when is_case_info_bool ci ->
        object_
          [ ("notation", string "if")
          ; ("cond", aux cond)
          ; ("args", list aux [dispatches.(0); dispatches.(1)]) ]
    | Case (ci, _, cond, dispatches) ->
        let k, idx = ci.ci_ind in
        let ind = MiniCic.Env.lookup_mutind env k in
        let cell = ind.cells.(idx) in
        object_
          [ ("notation", string "merge")
          ; ("cond", aux cond)
          ; ( "constructors"
            , array (fun x -> string (Name.get_id (fst x))) cell.cell_cons )
          ; ("args", array aux dispatches) ]
    | Var id ->
        dependency := Id.Set.add id !dependency ;
        string (unit_to_string c)
    | _ -> string (unit_to_string c)
  in
  let idx, inner = fetch_inner c 0 in
  let expr = aux inner in
  (idx, expr, !dependency)

let rec map_select_inner f c =
  match c with
  | App (op, [|t1; t2; inner|]) when is_const_fst op || is_const_snd op ->
      let inner' = map_select_inner f inner in
      if inner' == inner then c else App (op, [|t1; t2; inner'|])
  | _ ->
      let c' = f c in
      if c' == c then c else c'

module ConstrMap = Map.Make (MiniCic.Constr)
module IntSet = Set.Make (MiniCic.Int)

(* Add more vars for uncatched multi-return. *)
let add_temp_var_for_multi_ret env =
  let env' = ref env in
  let name_suffix = ref 0 in
  let multi_ret_map = ref ConstrMap.empty in
  let rec gen_name () =
    let name = "r" ^ string_of_int !name_suffix in
    name_suffix := !name_suffix + 1 ;
    if Id.Map.mem name !env'.env_named_context then gen_name () else name
  in
  (* Sort the vars. *)
  let id_body_list =
    Id.Map.fold
      (fun _ v statements ->
        match v with
        | LocalDef (id, body, t) ->
            let idx, _, _ = generate_expr env body in
            (id, idx, t, body) :: statements
        | _ -> statements )
      env.env_named_context []
  in
  (* Check the uncatched multi-return for var body. *)
  List.iter
    (fun (id, _, t, body) ->
      let rec aux c =
        if is_select c then (
          let c = map_select_inner (MiniCic.Constr.map aux) c in
          if ConstrMap.mem c !multi_ret_map then
            mkVar (ConstrMap.find c !multi_ret_map)
          else
            let id = gen_name () in
            multi_ret_map := ConstrMap.add c id !multi_ret_map ;
            let _, t, _ = parse_select c in
            env' := push_named (LocalDef (id, c, t)) !env' ;
            mkVar id )
        else MiniCic.Constr.map aux c
      in
      let body' = map_select_inner aux body in
      if MiniCic.Constr.compare body body' <> 0 then
        env' := push_named (LocalDef (id, body', t)) !env' ;
      if is_select body' then
        multi_ret_map := ConstrMap.add body' id !multi_ret_map )
    id_body_list ;
  (* For other leaked returns. *)
  let tuple_map =
    Id.Map.fold
      (fun _ v m ->
        match v with
        | LocalDef (_, body, _) when is_select body ->
            let idx, _, inner = parse_select body in
            ConstrMap.update inner
              (fun idx_map_opt ->
                match idx_map_opt with
                | None -> Some (IntSet.singleton idx)
                | Some idx_map -> Some (IntSet.add idx idx_map) )
              m
        | _ -> m )
      !env'.env_named_context ConstrMap.empty
  in
  ConstrMap.iter
    (fun tuple_body idx_set ->
      let tl = type_list_of_tuple_body !env' tuple_body in
      let _ =
        List.fold_left
          (fun idx t ->
            if IntSet.mem idx idx_set then idx + 1
            else
              let body = mk_select_with_type_list tl idx tuple_body in
              let id = gen_name () in
              env' := push_named (LocalDef (id, body, t)) !env' ;
              idx + 1 )
          0 tl
      in
      () )
    tuple_map ;
  !env'

(* If two var has same body, we assign one to the other *)
let dedup env =
  let env' = ref env in
  let _ =
    Id.Map.fold
      (fun _ v map ->
        match v with
        | LocalDef (id, body, t) ->
            if ConstrMap.mem body map then (
              env' :=
                push_named
                  (LocalDef (id, mkVar (ConstrMap.find body map), t))
                  !env' ;
              map )
            else ConstrMap.add body id map
        | _ -> map )
      env.env_named_context ConstrMap.empty
  in
  !env'

let generate_statement env statics params =
  let open! Json.Encode in
  let open TopoSort in
  let params_set =
    Id.Set.of_list
      (List.map (fun x -> Id.of_string x.name) (List.concat [params; statics]))
  in
  let statements =
    Id.Map.fold
      (fun _ v statements ->
        match v with
        | LocalDef (id, body, _) ->
            let idx, expr, deps = generate_expr env body in
            let deps =
              Id.Set.filter (fun e -> not (Id.Set.mem e params_set)) deps
            in
            ((id, idx, Json.stringify expr), deps) :: statements
        | _ -> statements )
      env.env_named_context []
    |> Array.of_list
    |> topo_sort (fun (id, _, _) deps -> Id.Set.remove id deps) Id.Set.cardinal
  in
  (* merge multiple-return statements *)
  let statements =
    Array.fold_left
      (fun stats (id, idx, expr) ->
        if
          List.find_opt (fun (_, expr') -> String.equal expr expr') stats
          = None
        then ([(id, idx)], expr) :: stats
        else
          List.map
            (fun (ids, expr') ->
              if String.compare expr expr' = 0 then ((id, idx) :: ids, expr)
              else (ids, expr') )
            stats )
      [] statements
    |> List.map (fun (ids, expr) ->
           let ids = Array.of_list ids in
           Array.fast_sort (fun (_, idx1) (_, idx2) -> idx1 - idx2) ids ;
           (Array.map fst ids, expr) )
  in
  let statements = List.rev statements in
  (* merge done *)
  list
    (fun (ids, expr) ->
      match Json.parse expr with
      | Some expr ->
          object_
            [ ("op", string "assign")
            ; ("left", array (fun id -> string (Id.to_string id)) ids)
            ; ("right", expr) ]
      | _ -> assert false )
    statements

let codegen_ast_json env =
  let env = add_temp_var_for_multi_ret env in
  let env = dedup env in
  let declare = generate_node_declare env in
  if List.length declare.returns = 0 then
    raise (Exceptions.CODEGEN_FAIL "no return value detected");
  let statements = generate_statement env declare.statics declare.params in
  let open! Json.Encode in
  let json =
    object_
      [("declares", encode_node_declare declare); ("statements", statements)]
  in
  Js.log (Json.stringify json) ;
  json
