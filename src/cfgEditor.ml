open Node
open MiniCic.CoreType
open MiniCic.Constr
open MiniCic.Names
open MiniCic.Context.Named.Declaration
open MiniCic.Prod

(* FIXME! how to make prod? *)
let c_case =
  MiniCic.Names.Constant.make core_dir (MiniCic.Names.Label.of_string "case")

module ConstrMap = Map.Make (MiniCic.Constr)

let generate_context_from_env prompt parent_div env =
  Js.log "build_cfg..." ;
  let constr_map = ref ConstrMap.empty in
  let ctxt = Context.init_context prompt parent_div Context.NodeMap.empty in
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
                Component.constant_to_node_with_params
                  (c, entry.entry_type, entry.info)
                  node_name inputs
              in
              ctxt.nodes
              <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes ;
              constr_map := ConstrMap.add e node_name !constr_map ;
              node_name
        in
        Some (mk_path node_name entry.info.(idx) true)
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
        let key, idx = ci.ci_ind in
        let node = Component.ind_to_node env key idx node_name in
        node.inputs.(0) <- {(node.inputs.(0)) with input= aux input_map 0 cond} ;
        Array.iteri
          (fun i c ->
            node.inputs.(i + 1)
            <- {(node.inputs.(i + 1)) with input= aux input_map 0 c} )
          args ;
        ctxt.nodes
        <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes ;
        Some (mk_path node_name ret_name true)
    | _ -> (* FIXME: This is not right *)
           assert false
  (* fold_with_full_binders push_local_def aux input_map 0 c*)
  and push_local_def c input_map =
    match c with
    | LocalDef (_, _, _) -> input_map
    | LocalAssum (_, _) -> input_map
  in
  let inputs =
    Id.Map.fold
      (fun _ v inputs ->
        match v with
        | LocalAssum (id, t) ->
            let name = Id.to_string id in
            let ret_name = Name.mk_name id in
            let input = Node.mk_node name (mkVar id) [||] [|(ret_name, t)|] in
            ctxt.nodes
            <- Context.NodeMap.add name (mk_graph_node input) ctxt.nodes ;
            Id.Map.add id (Some (mk_path name ret_name true)) inputs
        | LocalDef _ -> (* local var or retrun var *) inputs )
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
              Node.mk_node_export (Id.to_string id) (mkVar id) input
                [|(Name.Name id, typ)|]
            else
              Node.mk_node (Id.to_string id) (mkVar id) input
                [|(Name.Name id, typ)|]
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

let type_check_all_nodes node_map =
  let global_type_safe = ref true in
  Context.node_map_sorted_iter
    (fun _ node ->
      let node = DagreFFI.extract node in
      (* For Case, we update the input/output type to the first case branch *)
      match node.src with
      | Case (_, _, _, _) ->
          let default_typ = get_src_type node_map node.inputs.(1) in
          Array.iteri
            (fun i param ->
              if i > 0 then
                node.inputs.(i)
                <- {param with para_info= (fst param.para_info, default_typ)}
              )
            node.inputs ;
          Array.iteri
            (fun i (name, _) -> node.outputs.(i) <- (name, default_typ))
            node.outputs
      | _ ->
          () ;
          Array.iteri
            (fun i param ->
              let dst_type = snd param.para_info in
              let src_typ = get_src_type node_map param in
              let type_safe = MiniCic.Constr.compare src_typ dst_type = 0 in
              if not type_safe then global_type_safe := false ;
              let new_input =
                match param.input with
                | None ->
                    global_type_safe := false ;
                    None
                | Some (VAR (c, _)) -> Some (VAR (c, type_safe))
                | Some (PATH (in_node_name, ret_name, _)) ->
                    Some (PATH (in_node_name, ret_name, type_safe))
              in
              node.inputs.(i) <- {param with input= new_input} )
            node.inputs )
    node_map ;
  NodeShape.update_edges (Context.get_global_context ()) ;
  !global_type_safe

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
              | None -> assert false
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
            let env = if n.export then MiniCic.Env.export id env else env in
            (n.src, MiniCic.Env.push_named d env)
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

let generate_env_from_node_map node_map default_env =
  if type_check_all_nodes node_map then
    _generate_env_from_node_map node_map default_env
  else (
    Js.log "type check failed" ;
    assert false )

let build_cfg prompt tool_div parent_div env =
  Js.log env ;
  let ctxt = generate_context_from_env prompt parent_div env in
  let graph = DagreFFI.create_graph () in
  Context.init_layout graph ctxt.nodes ;
  Flowgraph.init_flowgraph env ctxt parent_div ;
  let constant_map =
    MiniCic.Env.fold_constants
      (fun c e acc -> Component.add_constant acc (c, e))
      (Component.mk_constant_map ())
      env
  in
  let ind_map =
    MiniCic.Env.fold_minds
      (fun c e acc -> Component.add_ind acc (c, e))
      (Component.mk_ind_map ()) env
  in
  Component.init_component_bar env ctxt tool_div constant_map ind_map
