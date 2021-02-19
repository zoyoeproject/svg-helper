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

let get_the_Name n =
  match n with Name.Name id -> Id.to_string id | _ -> assert false

let rec collect_outputs info idx acc c =
  let open MiniCic.Constr in
  match c with
  | App (f, [|a; b|]) when f = prod_type ->
      collect_outputs info (idx + 1) (acc @ [(info.(idx), a)]) b
  | _ -> acc @ [(info.(idx), c)]

let rec collect_params acc c =
  let open MiniCic.Constr in
  match c with
  | Prod (n, t, c) ->
      collect_params
        Node.({para_info= (get_the_Name n, t); input= None} :: acc)
        c
  | _ -> (acc, c)

let constant_to_node (c, typ, info) node_name category =
  let args, output_typ = collect_params [] typ in
  let outputs = collect_outputs info 0 [] output_typ in
  Node.mk_node node_name
    (App (mkConst c, [||]))
    (Array.of_list args) (Array.of_list outputs) category

let ind_to_node env info_key idx node_name category =
  let open MiniCic.Mind in
  (* Because case return type is depend on inputs, so we use `Int 0` for placeholder *)
  let ind = MiniCic.Env.lookup_mutind env info_key in
  let ind_cell = ind.cells.(idx) in
  let cases =
    Array.map
      (fun x -> {para_info= (Name.get_id (fst x), Int 0); input= None})
      ind_cell.cell_cons
  in
  let ci = MiniCic.Env.get_case_info env (info_key, idx) in
  let cond = {para_info= ("cond", mkInd ci.ci_ind); input= None} in
  Node.mk_node node_name
    (Case (ci, Int 0, Int 0, [||]))
    (Array.concat [[|cond|]; cases])
    [|(Name.Anonymous, Int 0)|]
    category

let constant_to_node_with_params (c, typ, info) node_name params category =
  let args, output_typ = collect_params [] typ in
  let args = List.mapi (fun i arg -> Node.{arg with input= params.(i)}) args in
  let outputs = collect_outputs info 0 [] output_typ in
  Node.mk_node node_name
    (App (mkConst c, [||]))
    (Array.of_list args) (Array.of_list outputs) category

let var_to_node (id, typ) node_name category =
  let inputs =
    match category with
    | CategoryVar
    | CategoryReturn -> [|Node.{para_info= ("i", typ); input= None}|]
    | _ -> [||]
  in
  Node.mk_node node_name (mkVar id) inputs [|(Name.Name id, typ)|] category

let constr_to_node env c node_name category typ =
  match c with
  | Const (c, _) ->
      Js.log @@ Label.to_string c ;
      let entry = MiniCic.Env.lookup_constant env c in
      constant_to_node (c, entry.entry_type, entry.info) node_name category
  | Ind (ind, _) ->
      let info_key, idx = ind in
      Js.log @@ Label.to_string info_key ;
      ind_to_node env info_key idx node_name category
  | Var id -> var_to_node (id, typ) node_name category
  | _ ->
      Js.log "false" ;
      assert false

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

let add_to_component_bar env context parent shift c =
  (* MiniCic.Constr.Int 0 is a place holder, it is useless *)
  let node = constr_to_node env c "" CategoryToolBox (Int 0) in
  let node_ele = Utils.mk_group_in parent None "" in
  draw_node_as_tool context node_ele node ;
  Document.setAttribute node_ele "class" "default" ;
  Utils.set_translate_matrix parent node_ele (!shift, 0) ;
  let prompt = Context.mk_var_promise context in
  Utils.on_mouseclick_set node_ele (fun _ ->
      if Context.toggle_focus context (Create (node_ele, (prompt, int_type)))
      then Utils.set_cfg_cursor context.cfg_ele (Document.outerHTML node_ele)
      else Utils.restore_cfg_cursor context.cfg_ele ) ;
  shift := !shift + 40

let init_component_bar env context parent contant_map ind_map =
  let open MiniCic.Env in
  let open MiniCic.Mind in
  let shift = ref 0 in
  ConstantMap.iter
    (fun k entry ->
      let node =
        constant_to_node
          (k, entry.entry_type, entry.info)
          "" Node.CategoryToolBox
      in
      let node_ele = Utils.mk_group_in parent None "" in
      let prompt = Context.mk_constant_promise (mkConst k) in
      draw_node_as_tool context node_ele node ;
      Document.setAttribute node_ele "class" "default" ;
      Utils.set_translate_matrix parent node_ele (!shift, 0) ;
      Utils.on_mouseclick_set node_ele (fun _ ->
          if
            Context.toggle_focus context
              (Create (node_ele, (prompt, entry.entry_type)))
          then
            Utils.set_cfg_cursor context.cfg_ele (Document.outerHTML node_ele)
          else Utils.restore_cfg_cursor context.cfg_ele ) ;
      shift := !shift + 50 )
    contant_map ;
  IndMap.iter
    (fun k (entry : inductive_block) ->
      Array.iteri
        (fun idx _ ->
          let node = ind_to_node env k idx "" Node.CategoryToolBox in
          let node_ele = Utils.mk_group_in parent None "" in
          let prompt = Context.mk_ind_promise (mkInd (k, idx)) in
          draw_node_as_tool context node_ele node ;
          Document.setAttribute node_ele "class" "default" ;
          Utils.set_translate_matrix parent node_ele (!shift, 0) ;
          Utils.on_mouseclick_set node_ele (fun _ ->
              (* FIXME: It seems the 3rd param of Create is useless, use Int 0 for placeholder. *)
              if
                Context.toggle_focus context
                  (Create (node_ele, (prompt, Int 0)))
              then
                Utils.set_cfg_cursor context.cfg_ele
                  (Document.outerHTML node_ele)
              else Utils.restore_cfg_cursor context.cfg_ele ) ;
          shift := !shift + 50 )
        entry.cells )
    ind_map ;
  add_to_component_bar env context parent shift (mkVar (Id.to_string "var"))
