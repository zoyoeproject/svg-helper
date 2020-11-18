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

let demo_cfg context parent =
  Flowgraph.init_flowgraph context parent

let demo_component context parent =
  let components = Component.mk_constant_map () in
  let components = Component.add_constant components (c_plus, c_intop_type) in
  let components = Component.add_constant components (c_minus, c_intop_type) in
  Component.init_component_bar context parent components
