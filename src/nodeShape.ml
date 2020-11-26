open Node
open Context
open MiniCic.Constr
open MiniCic.Names

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

let set_input_ancher context input item =
  Utils.on_mouseclick_set item (fun _ ->
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
  )

let set_output_ancher context item output =
  Utils.on_mouseclick_set item (fun _ ->
    Context.toggle_focus context (Connect (item, output))
  )

let draw_normal context parent node (cx, cy) (w,h) =
  let x1, y1 = cx - w/2, cy - h/2 in
  let x2, _ = cx + w/2, cy + h/2 in
  let text = Utils.mk_text "default" (x1, y1 - 2)
    (Constant.to_string (fst (Constr.destConst node.src))) in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let ax, ay = x1, (get_ancher y1 h (Array.length node.inputs) i) in
    let circle = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    let name, _ = input.para_info in
    let text = Utils.mk_text "default" (ax + 5, ay) name in
    set_input_ancher context input circle;
    (svg^text, i + 1)
  ) (text, 0) (node.inputs:param array) in
  let txt, _ = Array.fold_left (fun (svg, i) (output,typ) ->
    let circle = Circle.mk_circle_in parent "default" 3
      (x2, (get_ancher y1 h (Array.length node.outputs) i))
    in
    set_output_ancher context circle (Node.mk_path node.name output, typ);
    (svg, i + 1)
  ) (txt, 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

let draw_input context parent node (cx, cy) =
  let (w, h) = 30, 30 in
  let x1, y1 = cx - w/2, cy - h/2 in
  let x2, _ = cx + w/2, cy + h/2 in
  let text = Utils.mk_text "default" (x1, y1 - 2) (Id.to_string (destVar node.src)) in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (output,typ) ->
    let circle = Circle.mk_circle_in parent "default" 3
      (x2, (get_ancher y1 h (Array.length node.outputs) i))
    in
    set_output_ancher context circle (Node.mk_path node.name output, typ);
    (svg, i + 1)
  ) (text, 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

let draw_output context parent node (cx, cy) =
  let (w, h) = 30, 30 in
  let x1, y1 = cx - w/2, cy - h/2 in
  let text = Utils.mk_text "default" (x1, y1 - 2) (Id.to_string (destVar node.src)) in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let ax, ay = x1, (get_ancher y1 h (Array.length node.inputs) i) in
    let circle = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    let name, _ = input.para_info in
    let text = Utils.mk_text "default" (ax + 5, ay) name in
    set_input_ancher context input circle;
    (svg^text, i + 1)
  ) (text, 0) (node.inputs:param array) in
  ignore @@ Utils.mk_group_in parent None txt

