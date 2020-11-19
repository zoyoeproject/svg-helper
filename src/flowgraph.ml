open Node
open Context
let input_padding = 3

let draw_edges (nodes:node_map) =
  NodeMap.fold (fun _ (node:node) svg ->
    let edges, _ = Array.fold_left (fun ((svg:string), i) param ->
      let ix, iy = get_input_ancher node i in
      match param.input with
      | Some (PATH (n, output)) -> begin
        let src_node = NodeMap.find n nodes in
        let out_idx = find_output_idx output (DagreFFI.extract src_node) in
        let ox, oy = get_output_ancher src_node out_idx in
        let svg = svg ^ Arc.connect_horizontal "default-line"
            (Js.Int.toFloat ix, Js.Int.toFloat iy)
            (Js.Int.toFloat ox, Js.Int.toFloat oy) in
        (svg, i+1)
        end
      | Some (VAR n)-> begin
        let svg = svg ^ Arc.connect_horizontal "default-line"
            (Js.Int.toFloat (ix - 30), Js.Int.toFloat iy)
            (Js.Int.toFloat ix, Js.Int.toFloat iy) in
        let svg = svg ^ Utils.mk_text "default" (ix-30, iy-input_padding) n in
        (svg, i+1)
        end
      | _ -> (svg, i+1)
    ) ("", 0) node.extra.inputs in
    svg ^ edges
  ) nodes ""

let update_edges nodes node item =
  let parent = Document.get_by_id Document.document "edges" in
  let tsinfo = Utils.get_translate_info item in
  DagreFFI.(node.x <- fst tsinfo);
  DagreFFI.(node.y <- snd tsinfo);
  Document.setInnerHTML parent (draw_edges nodes)

let draw_node context parent node =
  let (cx, cy) = (0, 0) in
  let (w,h) = compute_size node in
  let x1, y1 = cx - w/2, cy - h/2 in
  let x2, _ = cx + w/2, cy + h/2 in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let ax, ay = x1, (get_ancher y1 h (Array.length node.inputs) i) in
    let circle = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    let name, _ = input.para_info in
    let text = Utils.mk_text "default" (ax + 5, ay) name in
    Utils.on_mouseclick_set circle (fun _ ->
      let _  = match input.input with
      | None -> begin
        match Context.get_focus_connect context with
          | Some (path, typ) ->
            input.input <- Some path
          | _ -> ()
        end
      | _ -> Js.log "input"; ()
      in
      let edges = Document.get_by_id Document.document "edges" in
      Document.setInnerHTML edges (draw_edges context.nodes)
    );
    (svg^text, i + 1)
  ) ("", 0) (node.inputs:param array) in
  let txt, _ = Array.fold_left (fun (svg, i) (output,typ) ->
    let circle = Circle.mk_circle_in parent "default" 3
      (x2, (get_ancher y1 h (Array.length node.outputs) i))
    in
    Utils.on_mouseclick_set circle (fun _ ->
      Context.toggle_focus context (Connect (circle, (Node.mk_path node.name output, typ)))
    );
    (svg, i + 1)
  ) (txt, 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

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
  ignore @@ Utils.mk_group_in parent (Some "edges") (draw_edges context.nodes)

let add_node context node (x,y) =
  let parent = Document.get_by_id Document.document "all" in
  let offx, offy = Utils.get_translate_info parent in
  let node_size = mk_graph_node node in
  node_size.x <- x - offx;
  node_size.y <- y - offy;
  context.nodes <- NodeMap.add node.name node_size context.nodes;
  reset context

let init_flowgraph context svgele =
  let all = Utils.mk_group_in svgele (Some "all") "" in
  Utils.init_dragdrop context svgele all;
  draw_nodes svgele all context;
  ignore @@ Utils.mk_group_in all (Some "edges") (draw_edges context.nodes);
  Utils.on_mouseclick_set svgele (fun e ->
    match Context.get_focus_create context with
    | Some (k, t) -> begin
        Context.clear_focus context;
        Utils.restore_cfg_cursor context "auto";
        Js.log @@ "create" ^ MiniCic.Names.Constant.to_string k;
        let node = Component.constr_to_node (k, t) (Context.new_ssa context) in
        add_node context node Document.(e.offsetX, e.offsetY);
        ()
      end
    | _ -> ()
  )

