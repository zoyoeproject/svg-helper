open Node
open MiniCic.CoreType
open MiniCic.Names
open MiniCic.Constr
module ConstantMap = Map.Make (Constant)
module IndMap = Map.Make (MutInd)

let mk_constant_map () = ConstantMap.empty

let mk_ind_map () = IndMap.empty

let add_constant constant_map (n, c) = ConstantMap.add n c constant_map

let add_ind ind_map (n, c) = IndMap.add n c ind_map

let get_name n =
  match n with Name.Name id -> Id.to_string id | _ -> assert false

let collect_constant_outputs entry c =
  let open MiniCic.Env in
  MiniCic.Prod.type_list_of_tuple_type c
  |> Array.of_list
  |> Array.mapi (fun i t -> (entry.info.(i), t))

let collect_constant_params entry =
  let open MiniCic.Env in
  let rec aux acc c =
    match c with
    | MiniCic.Constr.Prod (n, t, c) ->
        let acc = {para_info= (get_name n, t); input= None} :: acc in
        aux acc c
    | _ -> (acc, c)
  in
  let acc, c = aux [] entry.entry_type in
  (Array.of_list acc, c)

let constant_to_node (c, entry) node_name category =
  let args, output_typ = collect_constant_params entry in
  let outputs = collect_constant_outputs entry output_typ in
  Node.mk_node node_name (App (mkConst c, [||])) args outputs category

let constant_to_node_with_params (c, entry) node_name category params =
  let node = constant_to_node (c, entry) node_name category in
  Array.iteri (fun i input -> input.input <- params.(i)) node.inputs ;
  node

let ind_to_node env ind node_name category =
  let open MiniCic.Mind in
  (* Because case return type is depend on inputs, so we use `Int 0` for placeholder *)
  let info_key, idx = ind in
  let ind_info = MiniCic.Env.lookup_mutind env info_key in
  let ind_cell = ind_info.cells.(idx) in
  let cases =
    Array.map
      (fun x -> {para_info= (Name.get_id (fst x), Int 0); input= None})
      ind_cell.cell_cons
  in
  let ci = MiniCic.Env.get_case_info env ind in
  let cond = {para_info= ("cond", mkInd ci.ci_ind); input= None} in
  Node.mk_node node_name
    (Case (ci, Int 0, Int 0, [||]))
    (Array.concat [[|cond|]; cases])
    [|(Name.Anonymous, Int 0)|]
    category

let var_to_node (id, typ) node_name category =
  let inputs =
    match category with
    | CategoryVar | CategoryReturn ->
        [|Node.{para_info= ("i", typ); input= None}|]
    | _ -> [||]
  in
  Node.mk_node node_name (mkVar id) inputs [|(Name.Name id, typ)|] category

let node_constr_to_node env c node_name =
  match c with
  | Const (c, _) ->
      let entry = MiniCic.Env.lookup_constant env c in
      constant_to_node (c, entry) node_name CategoryFunction
  | Ind (ind, _) -> ind_to_node env ind node_name CategoryCase
  | _ -> assert false

let var_constr_to_node c node_name typ category =
  match c with
  | Var id -> var_to_node (id, typ) node_name category
  | _ -> assert false

let input_node c =
  Node.mk_node "input__unique" c [||]
    [|(Name.Anonymous, int_type)|]
    Node.CategoryToolBox

type component_bar =
  {container: Document.element; components: Node.t ConstantMap.t}

let font_size = 10

let draw_node_as_tool context parent node =
  let center = (25, 40) in
  let sz = compute_size node in
  if isVar node.src then NodeShape.draw_var context parent node center sz true
  else NodeShape.draw_normal context parent node center sz true

let add_to_component_bar context parent shift c =
  (* MiniCic.Constr.Int 0 is a placeholder, it is useless *)
  let node = var_constr_to_node c "" (Int 0) Node.CategoryToolBox in
  let node_ele = Utils.mk_group_in parent None "" in
  draw_node_as_tool context node_ele node ;
  Document.setAttribute node_ele "class" "default" ;
  Utils.set_translate_matrix parent node_ele (!shift, 0) ;
  Utils.on_mouseclick_set node_ele (fun _ ->
      if Context.toggle_focus context (Create (node_ele, CreatorVar)) then
        Utils.set_cfg_cursor context.cfg_ele (Document.outerHTML node_ele)
      else Utils.restore_cfg_cursor context.cfg_ele ) ;
  shift := !shift + 40

let init_component_bar env context parent contant_map ind_map =
  let open MiniCic.Mind in
  let shift = ref 0 in
  ConstantMap.iter
    (fun k entry ->
      let node = constant_to_node (k, entry) "" Node.CategoryToolBox in
      let node_ele = Utils.mk_group_in parent None "" in
      draw_node_as_tool context node_ele node ;
      Document.setAttribute node_ele "class" "default" ;
      Utils.set_translate_matrix parent node_ele (!shift, 0) ;
      Utils.on_mouseclick_set node_ele (fun _ ->
          if
            Context.toggle_focus context
              (Create (node_ele, CreatorNode (mkConst k)))
          then
            Utils.set_cfg_cursor context.cfg_ele (Document.outerHTML node_ele)
          else Utils.restore_cfg_cursor context.cfg_ele ) ;
      shift := !shift + 50 )
    contant_map ;
  IndMap.iter
    (fun k (entry : inductive_block) ->
      Array.iteri
        (fun idx _ ->
          let node = ind_to_node env (k, idx) "" Node.CategoryToolBox in
          let node_ele = Utils.mk_group_in parent None "" in
          draw_node_as_tool context node_ele node ;
          Document.setAttribute node_ele "class" "default" ;
          Utils.set_translate_matrix parent node_ele (!shift, 0) ;
          Utils.on_mouseclick_set node_ele (fun _ ->
              (* FIXME: It seems the 3rd param of Create is useless, use Int 0 for placeholder. *)
              if
                Context.toggle_focus context
                  (Create (node_ele, CreatorNode (mkInd (k, idx))))
              then
                Utils.set_cfg_cursor context.cfg_ele
                  (Document.outerHTML node_ele)
              else Utils.restore_cfg_cursor context.cfg_ele ) ;
          shift := !shift + 50 )
        entry.cells )
    ind_map ;
  add_to_component_bar context parent shift (mkVar (Id.to_string "var"))
