open Node
open MiniCic.Names
open Global
open MiniCic.CoreType
module Constr = MiniCic.Constr
let demo_cfg context parent =
  Flowgraph.init_flowgraph context parent

let c_demo = Constr.mkConst @@ Constant.make ModPath.initial (Label.of_string "demo")

let demo_component context parent =
  let components = Component.mk_constant_map () in
  let components = Component.add_constant components (c_plus, int_bop_type) in
  let components = Component.add_constant components (c_minus, int_bop_type) in
  Component.init_component_bar context parent components

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
  Context.init_context parent nodes


let init_context_with_constr parent =
  let open Constr in
  let x = Name.mk_name "x" in
  let y = Name.mk_name "y" in
  let app = mkApp (mkConstU (c_plus, 1), [|mkRel 1; mkRel 2|]) in
  let app = mkApp (mkConstU (c_plus, 1), [|mkRel 2; app|]) in
  let c = mkLambda (x, int_type, mkLambda (y, int_type, app)) in
  CfgEditor.build_cfg parent Global.basic_env c

