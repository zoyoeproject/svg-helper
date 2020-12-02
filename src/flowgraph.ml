open Node
open Context

let update_edges nodes node item =
  let parent = Document.get_by_id Document.document "edges" in
  let tsinfo = Utils.get_translate_info item in
  DagreFFI.(node.x <- fst tsinfo);
  DagreFFI.(node.y <- snd tsinfo);
  Document.setInnerHTML parent (NodeShape.draw_edges nodes)

let is_var_node node = match node.src with
  | MiniCic.Constr.Var _ -> true
  | _ -> false

let draw_node context parent node =
  let center = (0, 0) in
  let sz = compute_size node in
  if is_var_node node then
    match (Array.length node.inputs, Array.length node.outputs) with
    | 0, 0 -> assert false
    | _, 0 -> NodeShape.draw_output context parent node center sz
    | 0, _ -> NodeShape.draw_input context parent node center sz
    | _ -> NodeShape.draw_var context parent node center sz
  else
    NodeShape.draw_normal context parent node center sz

let draw_nodes svgele parent context =
  NodeMap.iter (fun node_name node ->
    let extra = DagreFFI.extract node in
    let item = Utils.mk_group_in parent (Some node_name) "" in
    Document.setAttribute item "class" "default";
    draw_node context item extra;
    Utils.set_translate_matrix svgele item (node.x, node.y);
    Utils.init_dragdrop_item svgele item (update_edges context.nodes node) context
  ) context.nodes

let reset context =
  let parent = Document.get_by_id Document.document "all" in
  Document.setInnerHTML parent "";
  draw_nodes context.cfg_ele parent context;
  ignore @@ Utils.mk_group_in parent (Some "edges") (NodeShape.draw_edges context.nodes)

let add_node context node (x,y) =
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
  draw_nodes svgele all context;
  ignore @@ Utils.mk_group_in all (Some "edges") (NodeShape.draw_edges context.nodes);
  Utils.on_mouseclick_set svgele (fun e ->
    match Context.get_focus_create context with
    | Some (c, t) -> begin
        Context.clear_focus context;
        Utils.restore_cfg_cursor context.cfg_ele;
        let node = Component.constr_to_node env c (Context.new_ssa context) in
        add_node context node Document.(e.offsetX, e.offsetY);
        ()
      end
    | _ -> ()
  )

