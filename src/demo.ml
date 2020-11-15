open Node
let basic_module_dir = Names.DirPath.make [Names.Id.of_string "core"]
let c_demo = Names.Constant.make Names.ModPath.initial (Names.Label.of_string "demo")
let c_int_type = Names.Constant.make Names.ModPath.initial (Names.Label.of_string "int")

let int_type = Constr.Const (c_int_type, 1)

let c_plus = Names.Constant.make Names.ModPath.initial (Names.Label.of_string "plus")
let c_minus = Names.Constant.make Names.ModPath.initial (Names.Label.of_string "minus")

let c_intop_type =
  let x = Names.Name.mk_name @@ Names.Id.of_string "x" in
  let y = Names.Name.mk_name @@ Names.Id.of_string "y" in
  Constr.mkProd (x, int_type, Constr.mkProd (y, int_type, int_type))

let demo_cfg parent =
  let x = Names.Name.mk_name "x" in
  let y = Names.Name.mk_name "y" in
  let a = Names.Name.mk_name "a" in
  let b = Names.Name.mk_name "b" in
  let c = Names.Name.mk_name "c" in
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
    |] [|Names.Name.Anonymous, int_type|];
  ] in
  Flowgraph.init_flowgraph parent nodes

let demo_component parent =
  let components = Component.mk_constant_map () in
  let components = Component.add_constant components (c_plus, c_intop_type) in
  let components = Component.add_constant components (c_minus, c_intop_type) in
  Component.init_component_bar parent components
