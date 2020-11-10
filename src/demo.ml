open Node
let demo_cfg parent =
  let graph = DagreFFI.create_graph () in
  let nodes = [
    Node.mk_node "n1" [|mk_param "i1" (Some (mk_var "i"))|] [|"x";"y"|];
    Node.mk_node "n2" [|mk_param "i2" (Some (mk_path "n1" "x"))|] [|"a"|];
    Node.mk_node "n3" [|mk_param "i3" (Some (mk_path "n1" "y"))|] [|"b"|];
    Node.mk_node "n4" [|mk_param "i4" (Some (mk_var "k"))|] [|"c"|];
    Node.mk_node "n5" [|
        mk_param "x" (Some (mk_path "n2" "a"));
        mk_param "y" (Some (mk_path "n3" "b"));
        mk_param "z" (Some (mk_path "n4" "c"));
    |] [|"result"|];
  ] in
  Flowgraph.init_graph graph nodes;
  Flowgraph.init_flowgraph parent graph
