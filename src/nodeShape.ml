open Node
open Context
open MiniCic.Constr
open MiniCic.Names

let input_padding = 3

let print_var = function
  | Var id -> Id.to_string id
  | Const (c, _) -> Constant.to_string c
  | Int i -> string_of_int i
  | App (Const (c, _), [||]) -> Constant.to_string c
  | Case (ci, _, _, _) -> "case-" ^ (MiniCic.Names.KerName.label (fst ci.ci_ind))
  | _ -> assert false

let draw_edges (nodes:node_map) =
  NodeMap.fold (fun _ (node:node) svg ->
    let edges, _ = Array.fold_left (fun ((svg:string), i) param ->
      let ix, iy = get_input_ancher node i in
      match param.input with
      | Some (PATH (n, output, type_safe)) -> begin
        let style = if type_safe then "default-line" else "error-line" in
        let src_node = NodeMap.find n nodes in
        let out_idx = find_output_idx output (DagreFFI.extract src_node) in
        let ox, oy = get_output_ancher src_node out_idx in
        let svg = svg ^ Arc.connect_horizontal style
            (Js.Int.toFloat ix, Js.Int.toFloat iy)
            (Js.Int.toFloat ox, Js.Int.toFloat oy) in
        (svg, i+1)
        end
      | Some (VAR (n, type_safe))-> begin
        let style = if type_safe then "default-line" else "error-line" in
        let svg = svg ^ Arc.connect_horizontal style
            (Js.Int.toFloat (ix - 30), Js.Int.toFloat iy)
            (Js.Int.toFloat ix, Js.Int.toFloat iy) in
        let svg = svg ^ Utils.mk_text "default" (ix-30, iy-input_padding) (print_var n) in
        (svg, i+1)
        end
      | _ -> (svg, i+1)
    ) ("", 0) node.extra.inputs in
    svg ^ edges
  ) nodes ""

let set_input_ancher context node_name input item =
  Utils.on_mousedoubleclick_set item (fun _ ->
    let _  = match input.input with
    | Some _ -> input.input <- None
    | _ -> ()
    in
    let edges = Document.get_by_id Document.document "edges" in
    Document.setInnerHTML edges (draw_edges context.nodes)
  );
  Utils.on_mouseclick_set item (fun _ ->
    let _, in_type = input.para_info in
    let _  = match input.input with
    | None -> begin
      match Context.get_focus_connect context with
        | Some (PATH (focused_node_name, na, _) , out_type (*typ*)) when focused_node_name != node_name ->
          Js.log focused_node_name;
          Js.log na;
          input.input <- Some (PATH (focused_node_name, na, Constr.compare in_type out_type = 0))
        | Some (VAR (n, _), out_type) ->
          input.input <- Some (VAR (n, Constr.compare in_type out_type = 0))
        | _ -> ()
      end
    | _ -> ()
    in
    let edges = Document.get_by_id Document.document "edges" in
    Document.setInnerHTML edges (draw_edges context.nodes)
  )

let set_output_ancher context item output =
  Utils.on_mouseclick_set item (fun _ ->
    Context.toggle_focus context (Connect (item, output))
  )

let draw_normal context parent node (cx, cy) (w,h) as_tool =
  let x1, y1 = cx - w/2, cy - h/2 in
  let x2, _ = cx + w/2, cy + h/2 in
  let text = Utils.mk_text "default" (x1, y1 - 2) (print_var node.src) in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let ax, ay = x1, (get_ancher y1 h (Array.length node.inputs) i) in
    let circle = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    let name, _ = input.para_info in
    let text = Utils.mk_text "default" (ax + 5, ay + 2) name in
    if (not as_tool) then set_input_ancher context node.name input circle;
    (svg^text, i + 1)
  ) (text, 0) (node.inputs:param array) in
  let txt, _ = Array.fold_left (fun (svg, i) (output,typ) ->
    let ax, ay = x2, (get_ancher y1 h (Array.length node.outputs) i) in
    let circle = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    let text = match output with
      | Name.Anonymous -> ""
      | Name.Name id -> Utils.mk_text "default" (ax + 5, ay + 2) (Id.to_string id)
    in
    if (not as_tool) then
      set_output_ancher context circle (Node.mk_path node.name output false, typ);
    (svg ^ text, i + 1)
  ) (txt, 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

let draw_input context parent node (cx, cy) (w, h) =
  let txt, _ = Array.fold_left (fun (svg, i) (output,typ) ->
    let circle = Circle.mk_circle_in parent "default" (w/2) (cx, cy) in
    let text = match output with
      | Name.Anonymous -> ""
      | Name.Name id -> Utils.mk_text "default" (cx + w/2, cy - h/2) (Id.to_string id)
    in
    set_output_ancher context circle (Node.mk_path node.name output false, typ);
    (svg ^ text, i + 1)
  ) ("", 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

let draw_output context parent node (cx, cy) (w, h) =
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let circle = Circle.mk_circle_in parent "default" (w/2) (cx, cy) in
    let name, _ = input.para_info in
    let text = Utils.mk_text "default" (cx - w/2 , cy - h/2) name in
    set_input_ancher context node.name input circle;
    (svg^text, i + 1)
  ) ("", 0) (node.inputs:param array) in
  ignore @@ Utils.mk_group_in parent None txt

let draw_var context parent node (cx, cy) (w,_) as_tool =
  let style = if node.export then "default-out" else "default" in
  let circle = Circle.mk_circle_in parent style (w/2) (cx, cy) in
  let text = Utils.mk_text "default" (cx, cy - 10) (print_var node.src) in
  if (Array.length node.inputs != 0) then begin
      let input = node.inputs.(0) in
      if (not as_tool) then set_input_ancher context node.name input circle
  end;
  if (Array.length node.outputs != 0) then begin
      let (output, typ) =  node.outputs.(0) in
      if (not as_tool) then set_output_ancher context circle (Node.mk_path node.name output false, typ)
  end;
  ignore @@ Utils.mk_group_in parent None text
