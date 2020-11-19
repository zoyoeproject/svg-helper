open Node
open MiniCic.Names
module Constr = MiniCic.Constr
let basic_module_dir = DirPath.make [Id.of_string "core"]
let c_demo = Constant.make ModPath.initial (Label.of_string "demo")
let c_int_type = Constant.make ModPath.initial (Label.of_string "int")

let int_type = Constr.Const (c_int_type, 1)

let c_plus = Constant.make ModPath.initial (Label.of_string "plus")
let c_minus = Constant.make ModPath.initial (Label.of_string "minus")

let c_intop_type =
  let x = Name.mk_name @@ Id.of_string "x" in
  let y = Name.mk_name @@ Id.of_string "y" in
  Constr.mkProd (x, int_type, Constr.mkProd (y, int_type, int_type))

let demo_cfg context parent =
  Flowgraph.init_flowgraph context parent

let demo_component context parent =
  let components = Component.mk_constant_map () in
  let components = Component.add_constant components (c_plus, c_intop_type) in
  let components = Component.add_constant components (c_minus, c_intop_type) in
  Component.init_component_bar context parent components

let build_cfg parent (c:MiniCic.Constr.t) =
  let open Constr in
  let ctxt = Utils.init_context parent Context.NodeMap.empty in
  let rec aux inputs acc c : var option =
    match c with
    | App (c, l) ->
      let inputs = Array.mapi (fun i c ->
        let input = aux inputs None c in
        mk_param ("i"^string_of_int i, int_type) input (* Change int type to the type of inputs *)
      ) l in
      let node_name = Context.new_ssa ctxt in
      let r = Name.mk_name "r" in
      let node = Node.mk_node node_name (fst @@ destConst c) inputs [|r, int_type|] in (* Change int type to the ret type of c *)
      (* Name.mk_name *)
      ctxt.nodes <- Context.NodeMap.add node_name (mk_graph_node node) ctxt.nodes;
      Some (mk_path node_name r)
    | Rel k -> List.nth inputs (k-1)
    | _ -> Constr.fold_with_full_binders push_local_def aux inputs None c
  and push_local_def c inputs = match c with
    | LocalDef (n, b, t) -> (aux inputs None b) :: inputs
    | LocalAssum (n, t) -> Some (mk_var (Name.to_string n)) :: inputs
  in
  let _ = fold_with_full_binders push_local_def aux [] None c in
  let graph = DagreFFI.create_graph () in
  Utils.init_graph graph ctxt.nodes;
  ctxt

let init_context parent =
  let x = Name.mk_name "x" in
  let y = Name.mk_name "y" in
  let a = Name.mk_name "a" in
  let b = Name.mk_name "b" in
  let c = Name.mk_name "c" in
  let nodes = [
    Node.mk_node "n1" c_demo [|mk_param ("i1",int_type) (Some (mk_var "i"))|]
      [|x, int_type ;
        y, int_type
      |];
    Node.mk_node "n2" c_demo [|mk_param ("i2",int_type) (Some (mk_path "n1" x))|] [|a, int_type|];
    Node.mk_node "n3" c_demo [|mk_param ("i3",int_type) (Some (mk_path "n1" y))|] [|b, int_type|];
    Node.mk_node "n4" c_demo [|mk_param ("i4",int_type) (Some (mk_var "k"))|] [|c, int_type|];
    Node.mk_node "n5" c_demo [|
        mk_param ("x", int_type) (Some (mk_path "n2" a));
        mk_param ("y", int_type) (None);
        mk_param ("z", int_type) (Some (mk_path "n4" c));
    |] [|Name.Anonymous, int_type|];
  ] in
  let nodes = List.fold_left (fun map n ->
    Context.NodeMap.add n.name (Node.mk_graph_node n) map
  ) Context.NodeMap.empty nodes in
  Utils.init_context parent nodes


let init_context_with_constr parent =
  let open Constr in
  let x = Name.mk_name "x" in
  let y = Name.mk_name "y" in
  let a = Name.mk_name "a" in
  let b = Name.mk_name "b" in
  let app = mkApp (mkConstU (c_plus, 1), [|mkRel 1; mkRel 2|]) in
  let app = mkApp (mkConstU (c_plus, 1), [|mkRel 2; app|]) in
  let c = mkLambda (x, int_type, mkLambda (y, int_type, app)) in
  build_cfg parent c

