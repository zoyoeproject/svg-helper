open Node
open MiniCic.CoreType

let build_cfg parent env (c:MiniCic.Constr.t) =
  let open MiniCic.Constr in
  let open MiniCic.Names in
  let open MiniCic.Context.Rel.Declaration in
  let ctxt = Context.init_context parent Context.NodeMap.empty in
  let rec aux inputs _ c : var option =
    Js.log "CHECK???";
    match c with
    | App (c, l) ->
      let inputs = Array.mapi (fun i c ->
        let input = aux inputs None c in
        mk_param ("i"^string_of_int i, int_type)
        (* MiniCic.Retype.type_of env c *) input (* Change int type to the type of inputs *)
      ) l in
      let node_name = Context.new_ssa ctxt in
      let r = Name.Anonymous in
      let node = Node.mk_node node_name c inputs [|r, int_type|] in (* Change int type to the ret type of c *)
      (* Name.mk_name *)
      ctxt.nodes <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes;
      Some (mk_path node_name r)
    | Rel k -> List.nth inputs (k-1)
    | Int n -> Some (mk_var c)
    | LetIn (na,b,t,c) -> aux (push_local_def (LocalDef (na,b,t)) inputs) None c
    | _ -> fold_with_full_binders push_local_def aux inputs None c
  and push_local_def c inputs = match c with
    | LocalDef (n, b, _) ->
        let name = Name.to_string n in
        Js.log "check???";
        let from = aux inputs None b in
        let input = [| mk_param ("i", int_type) from |] in
        let var_node = Node.mk_node name (mkVar (Id.of_string "input")) input [|n, int_type|] in
        ctxt.nodes <- Context.NodeMap.add name (mk_graph_node var_node) ctxt.nodes;
        Some (mk_path name n) :: inputs
    | LocalAssum (n, _) -> begin
        let name = Name.to_string n in
        let input = Node.mk_node name (mkVar (Id.of_string "input")) [||] [|n, int_type|] in
        ctxt.nodes <- Context.NodeMap.add name (mk_graph_node input) ctxt.nodes;
        Some (mk_path name n) :: inputs
      end
      (* Some (mk_var (Name.to_string n)) :: inputs *)
  in
  let output = fold_with_full_binders push_local_def aux [] None c in
  let inputs = [| mk_param ("i", int_type) output |] in
  let output_node = Node.mk_node (Context.new_ssa ctxt) (mkVar (Id.of_string "output"))  inputs [||] in
  ctxt.nodes <- Context.NodeMap.add (Context.new_ssa ctxt)  (mk_graph_node output_node) ctxt.nodes;
  let graph = DagreFFI.create_graph () in
  Context.init_layout graph ctxt.nodes;
  ctxt

