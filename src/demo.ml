open Node
let basic_module_dir = Names.DirPath.make [Names.Id.of_string "core"]
let c_int_type = Names.Constant.make Names.ModPath.initial (Names.Label.of_string "int")
let demo_cfg parent =
  let int_type = Constr.Const (c_int_type, 1) in
  let x = Names.Name.mk_name "x" in
  let y = Names.Name.mk_name "y" in
  let a = Names.Name.mk_name "a" in
  let b = Names.Name.mk_name "b" in
  let c = Names.Name.mk_name "c" in
  let nodes = [
    Node.mk_node "n1" [|mk_param ("i1",int_type) (Some (mk_var "i"))|]
      [|x, int_type ;
        y, int_type
      |];
    Node.mk_node "n2" [|mk_param ("i2",int_type) (Some (mk_path "n1" x))|] [|a, int_type|];
    Node.mk_node "n3" [|mk_param ("i3",int_type) (Some (mk_path "n1" y))|] [|b, int_type|];
    Node.mk_node "n4" [|mk_param ("i4",int_type) (Some (mk_var "k"))|] [|c, int_type|];
    Node.mk_node "n5" [|
        mk_param ("x", int_type) (Some (mk_path "n2" a));
        mk_param ("y", int_type) (None);
        mk_param ("z", int_type) (Some (mk_path "n4" c));
    |] [|Names.Name.Anonymous, int_type|];
  ] in
  Flowgraph.init_flowgraph parent nodes
