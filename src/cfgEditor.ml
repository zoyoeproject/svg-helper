open Node
open MiniCic.CoreType
open MiniCic.Constr
open MiniCic.Names
open MiniCic.Context.Named.Declaration
open MiniCic.Prod
open Exceptions

(* FIXME! how to make prod? *)
let c_case =
  MiniCic.Names.Constant.make core_dir (MiniCic.Names.Label.of_string "case")

module ConstrMap = Map.Make (MiniCic.Constr)

let get_id_of_var c =
  match c with
  | Var id -> id
  | _ -> assert false

let generate_context_from_env prompt parse_type parse_expr param_div parent_div env =
  Js.log "build_cfg..." ;
  let constr_map = ref ConstrMap.empty in
  let ctxt =
    Context.init_context prompt env parse_type parse_expr param_div parent_div
      Context.NodeMap.empty
  in
  (*
   * TODO we need to make sure c is a closed term
   * If c is not closed than we need to fill make it closed using
   * lambda x, c x
   *)
  let rec aux input_map idx e : var option =
    match e with
    | App (c, l) when c = fst_const -> aux input_map idx l.(2)
    | App (c, l) when c = snd_const -> aux input_map (idx + 1) l.(2)
    | App (c, l) when isConst c ->
        let inputs = Array.map (fun c -> aux input_map 0 c) l in
        let c = fst (destConst c) in
        let entry = MiniCic.Env.lookup_constant env c in
        let node_name =
          match ConstrMap.find_opt e !constr_map with
          | Some name -> name
          | None ->
              let node_name = Context.new_ssa ctxt in
              let node =
                Component.constant_to_node_with_params (c, entry) node_name
                  Node.CategoryFunction inputs
              in
              ctxt.nodes
              <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes ;
              constr_map := ConstrMap.add e node_name !constr_map ;
              node_name
        in
        Some (mk_path node_name entry.info.(idx) true)
    | App (c, l) when isVar c ->
        let inputs = Array.map (fun c -> aux input_map 0 c) l in
        let id = get_id_of_var c in
        let entry = MiniCic.Env.lookup_named id env in
        let typ = match entry with
        | LocalAssum (_, typ) -> typ
        | LocalDef (_, _, typ) -> typ
        in
        let node_name =
          match ConstrMap.find_opt e !constr_map with
          | Some name -> name
          | None ->
              let node_name = Context.new_ssa ctxt in
              let node =
                Component.type_to_node_with_params id typ node_name
                  Node.CategoryFunction inputs
              in
              ctxt.nodes
              <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes ;
              constr_map := ConstrMap.add e node_name !constr_map ;
              node_name
        in
        let names = Component.collect_type_output_names typ in
        Some (mk_path node_name names.(idx) true)
    | Int _ -> Some (mk_var e)
    | Var name -> (
        let name_info = MiniCic.Env.lookup_named name env in
        match name_info with
        | LocalDef (id, _, _) ->
            let name = Id.to_string id in
            let path = Some (mk_path name (Name id) true) in
            path
        | LocalAssum (id, _) -> (* mk_var c *)
                                Id.Map.find id input_map )
    | Case (ci, _, cond, args) ->
        let node_name = Context.new_ssa ctxt in
        let ret_name = Name.Anonymous in
        let node =
          Component.ind_to_node env ci.ci_ind node_name Node.CategoryCase
        in
        node.inputs.(0) <- {(node.inputs.(0)) with input= aux input_map 0 cond} ;
        Array.iteri
          (fun i c ->
            node.inputs.(i + 1)
            <- {(node.inputs.(i + 1)) with input= aux input_map 0 c} )
          args ;
        ctxt.nodes
        <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes ;
        Some (mk_path node_name ret_name true)
    | Const _ -> Some (mk_var e)
    | _ -> (* FIXME: This is not right *)
        Js.log (MiniCic.Pp.to_string e);
        assert false
  in
  let inputs =
    Id.Map.fold
      (fun _ v inputs ->
        match v with
        | LocalAssum (id, t) when not (isProd t) ->
            let name = Id.to_string id in
            let ret_name = Name.mk_name id in
            (* maybe error of cic-parser *)
            let static = Id.Set.mem id env.env_static in
            let category =
              if static then Node.CategoryStaticParameter
              else Node.CategoryParameter
            in
            let input =
              Node.mk_node name (mkVar id) [||] [|(ret_name, t)|] category
            in
            ctxt.nodes
            <- Context.NodeMap.add name (mk_graph_node input) ctxt.nodes ;
            Id.Map.add id (Some (mk_path name ret_name true)) inputs
        | _ -> inputs
      )
      env.env_named_context Id.Map.empty
  in
  Id.Map.iter
    (fun id _ ->
      Js.log "id" ;
      let name_info = MiniCic.Env.lookup_named id env in
      match name_info with
      | LocalDef (id, body, typ) ->
          let from = aux inputs 0 body in
          let input = [|mk_param ("i", typ) from|] in
          let local_node =
            if MiniCic.Env.is_exported id env then
              Node.mk_node (Id.to_string id) (mkVar id) input
                [|(Name.Name id, typ)|]
                Node.CategoryReturn
            else
              Node.mk_node (Id.to_string id) (mkVar id) input
                [|(Name.Name id, typ)|]
                Node.CategoryVar
          in
          ctxt.nodes
          <- Context.NodeMap.add (Id.to_string id) (mk_graph_node local_node)
               ctxt.nodes
      | LocalAssum (_, _) -> () )
    env.env_named_context ;
  ctxt

let option_get o = match o with None -> assert false | Some v -> v

(* Give a path of node_map, returns the index and type list *)
let count_output_index_of_path node_map node_name ret_name =
  let graph_node = Context.NodeMap.find node_name node_map in
  let node = DagreFFI.extract graph_node in
  let matched_index, _ =
    Array.fold_left
      (fun (index_opt, index) (name, _) ->
        if index_opt <> None then (index_opt, index + 1)
        else if Name.equal ret_name name then (Some index, index + 1)
        else (None, index + 1) )
      (None, 0) node.outputs
  in
  (option_get matched_index, Array.to_list (Array.map snd node.outputs))

let get_src_type node_map param =
  match param.input with
  | None -> Int 0 (* for placeholder *)
  | Some (VAR (c, _)) -> MiniCic.Retype.type_of MiniCic.Env.empty_env c
  | Some (PATH (in_node_name, ret_name, _)) ->
      let tuple_index, tuple_type_list =
        count_output_index_of_path node_map in_node_name ret_name
      in
      List.nth tuple_type_list tuple_index

let type_check_all_nodes ctxt =
  let open Context in
  let node_map = ctxt.nodes in
  let error = ref "" in
  (* For Case, we update the input/output type to the first case branch *)
  let update_case_type node =
    match node.src with
    | Case (_, _, _, _) ->
      let default_typ = get_src_type node_map node.inputs.(1) in
      Js.log default_typ ;
      Array.iteri
        (fun i param ->
          if i > 0 then
            (node.inputs.(i)).para_info
            <- (fst param.para_info, default_typ) )
        node.inputs ;
      Array.iteri
        (fun i (name, _) -> node.outputs.(i) <- (name, default_typ))
        node.outputs
    | _ -> ()
  in
  let update_input_type_safe node i param =
    let dst_type = snd param.para_info in
    let src_typ = get_src_type node_map param in
    let type_safe = MiniCic.Constr.compare src_typ dst_type = 0 in
    if not type_safe then
      error := ("Type check error for \"" ^ NodeShape.print_var node.src ^ "\"");
    let new_input =
      match param.input with
      | None ->
        error := "Miss input for \"" ^ NodeShape.print_var node.src ^ "\"";
        None
      | Some (VAR (c, _)) -> Some (VAR (c, type_safe))
      | Some (PATH (in_node_name, ret_name, _)) ->
          Some (PATH (in_node_name, ret_name, type_safe))
    in
    (node.inputs.(i)).input <- new_input ;
    if !error <> "" then
      raise (CODEGEN_FAIL !error)
  in
  let aux _ node =
    let node = DagreFFI.extract node in
    update_case_type node;
    Array.iteri (update_input_type_safe node) node.inputs
  in
  let () =
    try
      Context.node_map_sorted_iter aux node_map
    with e ->
      let () =
        match e with
        | TopoSort.TOPO_SORT_CIRCLE ->
          error := "circle detected"
        | _ -> ()
      in
      NodeShape.update_edges ctxt
  in
  !error

let _generate_env_from_node_map node_map default_env =
  let env = default_env in
  let size = Context.NodeMap.cardinal node_map in
  let cache = Hashtbl.create size in
  (* aux generate (body, type) constr of node n.
   * If n depends on any var, the var would be put into env.
   * return (body of node, filled env)
   *)
  let rec aux (graph_node : Node.t DagreFFI.node_size) env =
    let n = graph_node.extra in
    try
      let body = Hashtbl.find cache n.name in
      (body, env)
    with _ ->
      (* deal with args:
       * 1. initial array
       * 2. collect arg constr
       * 3. wrap with index
       *)
      let args = Array.make (Array.length n.inputs) (Int 0) in
      let env, _ =
        Array.fold_left
          (fun (env, i) param ->
            let body, env =
              match param.input with
              | None -> raise (CODEGEN_FAIL ("Miss input for " ^ NodeShape.print_var n.src))
              | Some (VAR (c, _)) -> (c, env)
              | Some (PATH (in_node_name, ret_name, _)) ->
                  let in_graph_node =
                    Context.NodeMap.find in_node_name node_map
                  in
                  let body, env = aux in_graph_node env in
                  let tuple_index, tuple_type_list =
                    count_output_index_of_path node_map in_node_name ret_name
                  in
                  let body =
                    mk_select_with_type_list tuple_type_list tuple_index body
                  in
                  (body, env)
            in
            let () = args.(i) <- body in
            (env, i + 1) )
          (env, 0) n.inputs
      in
      let body, env =
        match n.src with
        | Var id ->
            let typ = snd n.outputs.(0) in
            let d =
              if Array.length n.inputs = 0 then LocalAssum (id, typ)
              else
                let body = args.(0) in
                LocalDef (id, body, typ)
            in
            let env =
              if n.category = Node.CategoryReturn then
                MiniCic.Env.export id env
              else env
            in
            ( n.src
            , MiniCic.Env.push_named d
                ~static:(n.category = Node.CategoryStaticParameter)
                env )
        | App (c, [||]) -> (App (c, args), env)
        | Int _ -> (n.src, env)
        | Case (ci, _, _, [||]) ->
            let output_type =
              mk_prod_type (Array.to_list (Array.map snd n.outputs))
            in
            let app_args_len = Array.length args - 1 in
            let app_args = Array.sub args 1 app_args_len in
            let typ = Lambda (Anonymous, bool_type, output_type) in
            (Case (ci, typ, args.(0), app_args), env)
        | _ -> assert false
      in
      let () = Hashtbl.add cache n.name body in
      (body, env)
  in
  Context.NodeMap.fold
    (fun _ node env ->
      let _, env = aux node env in
      env )
    node_map env

let generate_env_from_node_map ctxt default_env =
  let error = type_check_all_nodes ctxt in
  if error = "" then
    _generate_env_from_node_map ctxt.nodes default_env
  else
    raise (CODEGEN_FAIL error)

let build_cfg prompt parse_type parse_expr tool_div param_div parent_div env =
  let ctxt =
    generate_context_from_env prompt parse_type parse_expr param_div parent_div env
  in
  let graph = DagreFFI.create_graph () in
  Context.init_layout graph ctxt.nodes ;
  Flowgraph.init_flowgraph env ctxt parent_div ;
  Component.init_component_bar env ctxt tool_div;
  Component.init_implicit_bar env ctxt param_div

let codegen_from_ctx () =
  let ctxt = Context.get_global_context () in
  let env = ctxt.env in
  let env = generate_env_from_node_map ctxt env in
  LustreCodegen.codegen_ast_json env
