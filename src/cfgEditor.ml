open Node
open MiniCic.CoreType

(* FIXME! how to make prod? *)
let c_case = MiniCic.Names.Constant.make core_dir (MiniCic.Names.Label.of_string "case")

let build_cfg parent env =
  let open MiniCic.Names in
  let open MiniCic.Constr in
  let ctxt = Context.init_context parent Context.NodeMap.empty in
  let rec aux input_map _ c : var option =
    match c with
    | App (c, l) ->
      let inputs = Array.mapi (fun i c ->
        let input = aux input_map None c in
        let input_type = MiniCic.Retype.type_of env c in
        mk_param ("i" ^ (string_of_int i), input_type) input
      ) l in
      let node_name = Context.new_ssa ctxt in
      let ret_name = Name.Anonymous in
      let ret_type = MiniCic.Retype.type_of env c in
      let node = Node.mk_node node_name c inputs [|ret_name, ret_type|] in
      (* Name.mk_name *)
      ctxt.nodes <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes;
      Some (mk_path node_name ret_name)
    | Int _ -> Some (mk_var c)
    | Var name -> begin
        let open MiniCic.Env in
        let name_info = lookup_named name env in
        match name_info with
        | LocalDef (n, _, _) ->
            let name = Id.to_string n in
            let path = Some (mk_path name (Name n)) in
            path
        | LocalAssum (id,_) -> (* mk_var c *)
            Id.Map.find id input_map
      end
    | Case (_, _, cond, args) ->
      let cond_input = aux input_map None cond in
      let cond_para = mk_param ("cond", bool_type) cond_input in
      let branch_inputs = Array.mapi (fun i c ->
          let input = aux input_map None c in
          let input_type = MiniCic.Retype.type_of env c in
           mk_param ("i"^string_of_int i, input_type) input
        ) args in
      let inputs = Array.of_list @@ cond_para :: (Array.to_list branch_inputs) in
      let node_name = Context.new_ssa ctxt in
      let ret_type = MiniCic.Retype.type_of env args.(0) in
      let ret_name = Name.Anonymous in
      let node = Node.mk_node node_name (mkConst c_case) (* FIXME: This is not correct *)
        inputs [|ret_name, ret_type|] in
      ctxt.nodes <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes;
      Some (mk_path node_name ret_name)
    | _ ->
      fold_with_full_binders push_local_def aux input_map None c
  and push_local_def c input_map  = match c with
    | LocalDef (_, _, _) -> input_map
    | LocalAssum (_, _) -> input_map
  in
  let open MiniCic.Context.Named.Declaration in
  let open MiniCic.Env in
  let inputs = Id.Map.fold (fun _ v inputs ->
      match v with
      | LocalAssum (id, t) ->
        let name = Id.to_string id in
        let ret_name = Name.mk_name id in
        let input = Node.mk_node name (mkVar id)
          [||] [|ret_name, t|] in
        ctxt.nodes <- Context.NodeMap.add name (mk_graph_node input) ctxt.nodes;
        Id.Map.add id (Some (mk_path name ret_name)) inputs
      | LocalDef _ -> (* local var or retrun var *) inputs
    ) env.env_named_context Id.Map.empty in
  Id.Map.iter (fun id _ ->
    let name_info = lookup_named id env in
    match name_info with
    | LocalDef (n,typ, body) ->
       let from = aux inputs None body in
       let input = [| mk_param ("i", typ) from |] in
       let local_node = Node.mk_node (Id.to_string n) (mkVar n) input
         [|Name.Name n, typ|] in
       ctxt.nodes <- Context.NodeMap.add (Id.to_string n) (mk_graph_node local_node)
         ctxt.nodes
    | LocalAssum (_,_) -> ()
  ) env.env_named_context;
  let graph = DagreFFI.create_graph () in
  Context.init_layout graph ctxt.nodes;
  ctxt

