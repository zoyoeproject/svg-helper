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

let rec collect_outputs info idx acc c =
  let open MiniCic.Constr in
  match c with
  | App (f, [|a; b|]) when f = prod_type ->
    collect_outputs info (idx+1) (acc @ [info.(idx), a]) c
  | _ -> acc @ [info.(idx), c]

let rec collect_params acc c =
  let open MiniCic.Constr in
  match c with
  | Prod (n, t, c) ->
    collect_params Node.({ para_info = (get_the_Name n, t); input = None } :: acc)
    c
  | _ -> acc, c

let constant_to_node (c, typ, info) node_name =
  let args, output_typ = collect_params [] typ in
  let outputs = collect_outputs info 0 [] output_typ in
  Node.mk_node node_name (App (mkConst c, [||])) (Array.of_list args) (Array.of_list outputs)

let var_to_node (id, typ) node_name =
  let arg = Node.({
    para_info = "i", Evar ("input_type", [||]);
    input = None
  }) in
  Node.mk_node node_name (mkVar id) [|arg|] [|Name.Name id, typ|]

let constr_to_node env c node_name =
  match c with
  | Const (c, _) -> begin
      Js.log @@ Label.to_string c;
      let entry = MiniCic.Env.lookup_constant env c in
      constant_to_node (c, entry.entry_type, entry.info) node_name
    end
  | Var id -> var_to_node (id, Evar ("input_type", [||])) node_name
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

let draw_node_as_tool context parent node =
  let center = (20, 30) in
  let sz = compute_size node in
  if (isVar node.src) then
    NodeShape.draw_var context parent node center sz true
  else
    NodeShape.draw_normal context parent node center sz true

let add_to_component_bar env context parent shift c =
  let node = constr_to_node env c "" in
  let node_ele = Utils.mk_group_in parent None "" in
  draw_node_as_tool context node_ele node;
  Document.setAttribute node_ele "class" "default";
  Utils.set_translate_matrix parent node_ele (!shift, 0);
  let prompt = Context.mk_var_promise context in
  Utils.on_mouseclick_set node_ele (fun _ ->
    if Context.toggle_focus context
      (Create (node_ele, (prompt, int_type)))
    then
      Utils.set_cfg_cursor context.cfg_ele
        (Document.outerHTML node_ele)
    else
      Utils.restore_cfg_cursor context.cfg_ele
  );
  shift := !shift + 40

let init_component_bar env context parent components =
  let open MiniCic.Env in
  let shift = ref 0 in
  ConstantMap.iter (fun k entry ->
    let node = constant_to_node (k, entry.entry_type, entry.info) "" in
    let node_ele = Utils.mk_group_in parent None "" in
    let prompt = Context.mk_constant_promise context (mkConst k) in
    draw_node_as_tool context node_ele node;
    Document.setAttribute node_ele "class" "default";
    Utils.set_translate_matrix parent node_ele (!shift, 0);
    Utils.on_mouseclick_set node_ele (fun _ ->
      if Context.toggle_focus context (Create (node_ele, (prompt, entry.entry_type))) then
        Utils.set_cfg_cursor context.cfg_ele
          (Document.outerHTML node_ele)
      else
        Utils.restore_cfg_cursor context.cfg_ele
    );
    shift := !shift + 40
 ) components;
 add_to_component_bar env context parent shift (mkVar (Id.to_string "var"))
