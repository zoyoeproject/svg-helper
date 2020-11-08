open Node

let build_edges graph nodes =
  List.iter (fun node ->
    let src = node.name in
    Array.iter (fun input ->
      match input with
      | PATH (node_name, _) -> DagreFFI.add_edge graph node_name src
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
    let edges, _ = Array.fold_left (fun (svg, i) input ->
      let ix, iy = get_input_ancher node i in
      match input with
      | PATH (n, output) -> begin
        let src_node = DagreFFI.get_node graph n in
        let out_idx = find_output_idx output src_node.extra in
        let ox, oy = get_output_ancher src_node out_idx in
        let svg = svg ^ Arc.connect_horizontal "default"
            (Js.Int.toFloat ix, Js.Int.toFloat iy)
            (Js.Int.toFloat ox, Js.Int.toFloat oy) in
        (svg, i+1)
        end
      | _ -> (svg, i+1)
    ) ("", 0) node.extra.inputs in
    svg ^ edges
  ) "" (DagreFFI.nodes graph)

let draw_nodes graph =
  Array.fold_left (fun svg node_name ->
    let node = DagreFFI.get_node graph node_name in
    let extra = DagreFFI.extract node in
    svg ^ (draw_node extra (node.x, node.y))
  ) "" (DagreFFI.nodes graph)

let draw_graph graph =
  (draw_nodes graph) ^ (draw_edges graph)


