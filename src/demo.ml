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

let init_context_with_constr parent =
  let open Constr in
  let x = Name.mk_name "x" in
  let y = Name.mk_name "y" in
  let app = mkApp (mkConstU (c_plus, 1), [|mkRel 1; mkRel 2|]) in
  let app = mkApp (mkConstU (c_plus, 1), [|mkRel 2; app|]) in
  let c = mkLambda (x, int_type, mkLambda (y, int_type, app)) in
  CfgEditor.build_cfg parent Global.basic_env c

