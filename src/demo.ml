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
  let x = Id.of_string "x" in
  let y = Id.of_string "y" in
  let z = Id.of_string "z" in
  let app = mkApp (mkConstU (c_plus, 1), [|Constr.mkVar x; Constr.mkVar y|]) in
  let app = mkApp (mkConstU (c_plus, 1), [|Constr.mkVar z; app|]) in
  let n = Constr.mkApp (mkConstU (c_minus, 1), [|Int 1; Int 2|]) in
  let r = Id.of_string "r" in
  let n' = Id.of_string "n" in
  let env = Global.basic_env in
  let env = env
    |> MiniCic.Env.push_named (LocalAssum (x, int_type))
    |> MiniCic.Env.push_named (LocalAssum (y, int_type))
    |> MiniCic.Env.push_named (LocalAssum (z, int_type))
    |> MiniCic.Env.push_named (LocalDef (r, app, int_type))
    |> MiniCic.Env.push_named (LocalDef (n', n, int_type))
  in
  CfgEditor.build_cfg parent env

