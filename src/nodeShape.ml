open Node
open Context
open MiniCic.Constr
open MiniCic.Names

let input_padding = 3

let print_var c =
  match c with
  | Var id -> Id.to_string id
  | Const (c, _) -> Constant.to_string c
  | Int i -> string_of_int i
  | App (Const (c, _), [||]) -> Constant.to_string c
  | App (Var c, [||]) -> Id.to_string c
  | Case (ci, _, _, _) -> "case-" ^ MiniCic.Names.KerName.label (fst ci.ci_ind)
  | _ -> Js.log (MiniCic.Pp.to_string c); assert false

let draw_edges (nodes : node_map) =
  NodeMap.fold
    (fun _ (node : node) svg ->
      let edges, _ =
        Array.fold_left
          (fun ((svg : string), i) param ->
            let ix, iy = get_input_ancher node i in
            match param.input with
            | Some (PATH (n, output, type_safe)) ->
                let style =
                  if type_safe then "default-line" else "error-line"
                in
                let src_node = NodeMap.find n nodes in
                let out_idx =
                  find_output_idx output (DagreFFI.extract src_node)
                in
                let ox, oy = get_output_ancher src_node out_idx in
                let svg =
                  svg
                  ^ Arc.connect_horizontal style
                      (Js.Int.toFloat ix, Js.Int.toFloat iy)
                      (Js.Int.toFloat ox, Js.Int.toFloat oy)
                in
                (svg, i + 1)
            | Some (VAR (n, type_safe)) ->
                let style =
                  if type_safe then "default-line" else "error-line"
                in
                let svg =
                  svg
                  ^ Arc.connect_horizontal style
                      (Js.Int.toFloat (ix - 30), Js.Int.toFloat iy)
                      (Js.Int.toFloat ix, Js.Int.toFloat iy)
                in
                let svg =
                  svg
                  ^ Utils.mk_text "default"
                      (ix - 30, iy - input_padding)
                      (print_var n)
                in
                (svg, i + 1)
            | _ -> (svg, i + 1) )
          ("", 0) node.extra.inputs
      in
      svg ^ edges )
    nodes ""

let update_edges context =
  let edges = Document.get_by_id Document.document "edges" in
  Document.setInnerHTML edges (draw_edges context.nodes)

let draw_normal context parent node (cx, cy) (w, h) as_tool set_input_ancher
    set_output_ancher =
  let x1, y1 = (cx - (w / 2), cy - (h / 2)) in
  let x2, _ = (cx + (w / 2), cy + (h / 2)) in
  let text = Utils.mk_text "default" (x1, y1 - 4) (print_var node.src) in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w, h) (x1, y1) ;
  let txt, _ =
    Array.fold_left
      (fun (svg, i) (input : param) ->
        let ax, ay = (x1, get_ancher y1 h (Array.length node.inputs) i) in
        let circle = Circle.mk_circle parent "default" 3 (ax, ay) in
        let name, _ = input.para_info in
        let text = Utils.mk_text "default" (ax + 5, ay + 2) name in
        if not as_tool then set_input_ancher context node.name input circle ;
        (svg ^ text, i + 1) )
      (text, 0)
      (node.inputs : param array)
  in
  let txt, _ =
    Array.fold_left
      (fun (svg, i) (output, typ) ->
        let ax, ay = (x2, get_ancher y1 h (Array.length node.outputs) i) in
        let circle = Circle.mk_circle parent "default" 3 (ax, ay) in
        let text =
          match output with
          | Name.Anonymous -> ""
          | Name.Name id ->
              Utils.mk_text "default" (ax - 10, ay + 2) (Id.to_string id)
        in
        if not as_tool then
          set_output_ancher context circle
            (Node.mk_path node.name output false, typ) ;
        (svg ^ text, i + 1) )
      (txt, 0) node.outputs
  in
  ignore @@ Utils.mk_group_in parent None txt

let draw_input context parent node (cx, cy) (w, h) set_output_ancher =
  let txt, _ =
    Array.fold_left
      (fun (svg, i) (output, typ) ->
        let circle = Circle.mk_circle parent "default" (w / 2) (cx, cy) in
        let text =
          match output with
          | Name.Anonymous -> ""
          | Name.Name id ->
              Utils.mk_text "default"
                (cx + (w / 2), cy - (h / 2))
                (Id.to_string id)
        in
        set_output_ancher context circle
          (Node.mk_path node.name output false, typ) ;
        (svg ^ text, i + 1) )
      ("", 0) node.outputs
  in
  ignore @@ Utils.mk_group_in parent None txt

let draw_output context parent node (cx, cy) (w, h) set_input_ancher =
  let txt, _ =
    Array.fold_left
      (fun (svg, i) (input : param) ->
        let circle = Circle.mk_circle parent "default" (w / 2) (cx, cy) in
        let name, _ = input.para_info in
        let text = Utils.mk_text "default" (cx - (w / 2), cy - (h / 2)) name in
        set_input_ancher context node.name input circle ;
        (svg ^ text, i + 1) )
      ("", 0)
      (node.inputs : param array)
  in
  ignore @@ Utils.mk_group_in parent None txt

let draw_var context parent node (cx, cy) (w, _) as_tool set_var_ancher =
  let style =
    match node.category with
    | CategoryParameter -> "default-in"
    | CategoryReturn -> "default-out"
    | CategoryStaticParameter -> "default-static"
    | _ -> "default"
  in
  let circle = Circle.mk_circle parent style (w / 2) (cx, cy) in
  let text = Utils.mk_text "default" (cx, cy - 10) (print_var node.src) in
  if not as_tool then set_var_ancher context node circle ;
  ignore @@ Utils.mk_group_in parent None text

let update_var_style node item =
  let style =
    match node.category with
    | CategoryParameter -> "default-in"
    | CategoryReturn -> "default-out"
    | CategoryStaticParameter -> "default-static"
    | _ -> "default"
  in
  item |. Document.setAttribute "class" style
