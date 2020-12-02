open Node
open MiniCic.CoreType
open MiniCic.Names
open MiniCic.Constr

module ConstantMap =  Map.Make(Constant)

let mk_constant_map () = ConstantMap.empty

let add_constant context (n, c) = ConstantMap.add n c context

let get_the_Name n = match n with
  | Name.Name id -> Id.to_string id
  | _ -> assert false

let rec collect_params acc c =
  let open MiniCic.Constr in
  match c with
  | Prod (n, t, c) ->
    collect_params Node.({ para_info = (get_the_Name n, t); input = None } :: acc)
    c
  | _ -> acc, c

let constant_to_node (c, typ) node_name =
  let args, output_typ = collect_params [] typ in
  Node.mk_node node_name (mkConst c) (Array.of_list args) [|Name.Anonymous, output_typ|]

let var_to_node (id, typ) node_name =
  Node.mk_node node_name (mkVar id) [||] [|Name.Name id, typ|]

let constr_to_node env c node_name =
  match c with
  | Const (c, _) -> begin
      Js.log @@ Label.to_string c;
      let entry = MiniCic.Env.lookup_constant env c in
      constant_to_node (c, entry.entry_type) node_name
    end
  | Var id -> var_to_node (id, Evar (1, [||])) node_name
  | _ -> begin
      Js.log "false";
      assert false
    end

let input_node c =
  Node.mk_node "input__unique" c [||] [|Name.Anonymous, int_type|]

type component_bar = {
  container: Document.element;
  components: Node.t ConstantMap.t;
}

let font_size = 10

let draw_node_as_tool parent node =
  let (w,h) = compute_size node in
  let (cx, cy) = (w/2 + 10, h/2 + 10 + font_size) in
  let x1, y1 = cx - w/2, cy - h/2 in
  let x2, _ = cx + w/2, cy + h/2 in
  let text = Utils.mk_text "default" (x1, y1 - 2) (NodeShape.print_var node.src) in
  ignore @@ Polygon.mk_rectangle_in parent "default" (w,h) (x1,y1);
  let txt, _ = Array.fold_left (fun (svg, i) (input:param) ->
    let ax, ay = x1, (get_ancher y1 h (Array.length node.inputs) i) in
    let _ = Circle.mk_circle_in parent "default" 3 (ax, ay) in
    let name, _ = input.para_info in
    let text = Utils.mk_text "default" (ax + 5, ay) name in
    (svg^text, i + 1)
  ) (text, 0) (node.inputs:param array) in
  let txt, _ = Array.fold_left (fun (svg, i) _ ->
    let _ = Circle.mk_circle_in parent "default" 3
      (x2, (get_ancher y1 h (Array.length node.outputs) i))
    in
    (svg, i + 1)
  ) (txt, 0) node.outputs in
  ignore @@ Utils.mk_group_in parent None txt

let add_to_component_bar env context parent shift c =
 let node = constr_to_node env c "" in
 let node_ele = Utils.mk_group_in parent None "" in
 draw_node_as_tool node_ele node;
 Document.setAttribute node_ele "class" "default";
 Utils.set_translate_matrix parent node_ele (!shift, 0);
 Utils.on_mouseclick_set node_ele (fun _ ->
   if Context.toggle_focus context (Create (node_ele, (c, int_type))) then
     Utils.set_cfg_cursor context.cfg_ele
       (Document.outerHTML node_ele)
   else
     Utils.restore_cfg_cursor context.cfg_ele
 );
 shift := !shift + 40

let init_component_bar env context parent components =
  let shift = ref 0 in
  ConstantMap.iter (fun k t ->
    let node = constant_to_node (k, t) "" in
    let node_ele = Utils.mk_group_in parent None "" in
    draw_node_as_tool node_ele node;
    Document.setAttribute node_ele "class" "default";
    Utils.set_translate_matrix parent node_ele (!shift, 0);
    Utils.on_mouseclick_set node_ele (fun _ ->
      if Context.toggle_focus context (Create (node_ele, (mkConst k, t))) then
        Utils.set_cfg_cursor context.cfg_ele
          (Document.outerHTML node_ele)
      else
        Utils.restore_cfg_cursor context.cfg_ele
    );
    shift := !shift + 40
 ) components;
 add_to_component_bar env context parent shift (mkVar (Id.to_string "x"))
