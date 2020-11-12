open Node

let build_edges graph nodes =
  List.iter (fun node ->
    let src = node.name in
    Array.iter (fun param ->
      match param.input with
      | Some (PATH (node_name, _)) -> DagreFFI.add_edge graph node_name src
      | _ -> ()
    ) node.inputs
  ) nodes

let init_graph graph nodes =
  List.iter (fun node ->
    DagreFFI.add_node graph node.name (mk_graph_node node)
  ) nodes;
  build_edges graph nodes;
  DagreFFI.layout graph

let draw_edges graph =
  Array.fold_left (fun svg node_name ->
    let node = DagreFFI.get_node graph node_name in
    let edges, _ = Array.fold_left (fun (svg, i) param ->
      let ix, iy = get_input_ancher node i in
      match param.input with
      | Some (PATH (n, output)) -> begin
        let src_node = DagreFFI.get_node graph n in
        let out_idx = find_output_idx output src_node.extra in
        let ox, oy = get_output_ancher src_node out_idx in
        let svg = svg ^ Arc.connect_horizontal "default-line"
            (Js.Int.toFloat ix, Js.Int.toFloat iy)
            (Js.Int.toFloat ox, Js.Int.toFloat oy) in
        (svg, i+1)
        end
      | _ -> (svg, i+1)
    ) ("", 0) node.extra.inputs in
    svg ^ edges
  ) "" (DagreFFI.nodes graph)

let update_edges graph node item =
  let parent = Document.get_by_id Document.document "edges" in
  let tsinfo = Utils.get_translate_info item in
  DagreFFI.(node.x <- fst tsinfo);
  DagreFFI.(node.y <- snd tsinfo);
  Document.setInnerHTML parent (draw_edges graph)

let draw_node parent (node:node) =
  let (cx, cy) = (0, 0) in
  let (w,h) = compute_size node in
  let x1, y1 = cx - w/2, cy - h/2 in
  let x2, _ = cx + w/2, cy + h/2 in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let ax, ay = x1, (get_ancher y1 h (Array.length node.inputs) i) in
    let circle = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    Utils.on_mouseclick_set circle (fun _ ->
      Document.setAttribute circle "class" "focus"
    );
    let text = Utils.mk_text "default" (ax + 5, ay) input.name in
    (svg^text, i + 1)
  ) ("", 0) (node.inputs:param array) in
  let txt, _ = Array.fold_left (fun (svg, i) _ ->
    ignore @@ Circle.mk_circle_in parent "default" 3
      (x2, (get_ancher y1 h (Array.length node.outputs) i));
    (svg, i + 1)
  ) (txt, 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

let draw_nodes svgele parent graph context =
  Array.iter (fun node_name ->
    let node = DagreFFI.get_node graph node_name in
    let extra = DagreFFI.extract node in
    let item = Utils.mk_group_in parent (Some node_name) "" in
    draw_node item extra;
    Utils.set_translate_matrix svgele item (node.x, node.y);
    Utils.init_dragdrop_item svgele item (update_edges graph node) context
  ) (DagreFFI.nodes graph)

let init_flowgraph svgele nodes =
  let graph = DagreFFI.create_graph () in
  init_graph graph nodes;
  let all = Utils.mk_group_in svgele (Some "all") "" in
  let context = Utils.init_dragdrop svgele all in
  draw_nodes svgele all graph context;
  ignore @@ Utils.mk_group_in all (Some "edges") (draw_edges graph)

