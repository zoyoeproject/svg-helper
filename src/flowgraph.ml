open Node
open Context

let update_edges nodes node item =
  let parent = Document.get_by_id Document.document "edges" in
  let tsinfo = Utils.get_translate_info item in
  DagreFFI.(node.x <- fst tsinfo);
  DagreFFI.(node.y <- snd tsinfo);
  Document.setInnerHTML parent (NodeShape.draw_edges nodes)

let is_var_node node =
  match node.src with MiniCic.Constr.Var _ -> true | _ -> false

let draw_node context parent node =
  let center = (0, 0) in
  let sz = compute_size node in
  if is_var_node node then
    NodeShape.draw_var context parent node center sz false
      Component.set_var_ancher
  else
    NodeShape.draw_normal context parent node center sz false
      Component.set_input_ancher Component.set_output_ancher

let draw_nodes svgele parent context =
  NodeMap.iter
    (fun node_name node ->
      let extra = DagreFFI.extract node in
      let item = Utils.mk_group_in parent (Some node_name) "" in
      Document.setAttribute item "class" "default";
      draw_node context item extra;
      Utils.set_translate_matrix svgele item (node.x, node.y);
      Utils.init_dragdrop_item svgele item
        (update_edges context.nodes node)
        context)
    context.nodes

let reset context =
  let parent = Document.get_by_id Document.document "all" in
  Document.setInnerHTML parent "";
  ignore
  @@ Utils.mk_group_in parent (Some "edges")
       (NodeShape.draw_edges context.nodes);
  draw_nodes context.cfg_ele parent context

let add_node context node (x, y) =
  let parent = Document.get_by_id Document.document "all" in
  let offx, offy = Utils.get_translate_info parent in
  let node_size = mk_graph_node node in
  node_size.x <- x - offx;
  node_size.y <- y - offy;
  context.nodes <- NodeMap.add node.name node_size context.nodes;
  reset context

let init_flowgraph env context svgele =
  let all = Utils.mk_group_in svgele (Some "all") "" in
  Utils.init_dragdrop context svgele all;
  ignore
  @@ Utils.mk_group_in all (Some "edges") (NodeShape.draw_edges context.nodes);
  draw_nodes svgele all context;
  Utils.on_mouseclick_set svgele (fun e ->
      match Context.get_focus_create context with
      | Some creator -> (
          Context.clear_focus context;
          Utils.restore_cfg_cursor context.cfg_ele;
          match creator with
          | CreatorNode c ->
              let node =
                Component.creator_node_to_node env c (Context.new_ssa context)
              in
              add_node context node Document.(e.offsetX, e.offsetY)
          | CreatorVar ->
              context.prompt "Add Variable"
                [|
                  { label = "var name"; info = "text"; default = "" };
                  {
                    label = "category";
                    info = "static parameter|parameter|var|return";
                    default = "var";
                  };
                  { label = "type"; info = "text"; default = "" };
                |]
                (fun args ->
                  let category =
                    List.find
                      (fun (str, _) -> str = args.(1))
                      Node.category_string_pairs
                    |> snd
                  in
                  let typ =
                    try context.parse_type args.(2) context.env
                    with _ ->
                      raise
                        (Exceptions.CFG_ERROR
                           ("Failed to parse type \"" ^ args.(2) ^ "\""))
                  in
                  match NodeMap.find_opt args.(0) context.nodes with
                  | Some _ ->
                      raise
                        (Exceptions.CFG_ERROR
                           ("\"" ^ args.(0) ^ "\" already exists"))
                  | None ->
                      if category = Node.CategoryStaticParameter then
                        context.env <-
                          MiniCic.Env.push_named
                            (LocalAssum (args.(0), typ))
                            ~static:true context.env;
                      let node =
                        Component.var_constr_to_node
                          (Constr.mkVar (Names.Id.to_string args.(0)))
                          (Context.new_ssa context) typ category
                      in
                      add_node context node Document.(e.offsetX, e.offsetY)))
      | _ -> ());
  Utils.on_contextmenu_set svgele (fun e ->
      match Context.get_focus_create context with
      | Some _ ->
          Context.clear_focus context;
          Utils.restore_cfg_cursor context.cfg_ele;
          Document.stopPropagation e;
          Document.preventDefault e
      | _ -> ())
