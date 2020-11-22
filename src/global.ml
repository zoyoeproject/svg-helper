(* Demo constants *)
open MiniCic.Names
open MiniCic.CoreType
module Constr = MiniCic.Constr

let c_plus = Constant.make ModPath.initial (Label.of_string "plus")
let c_minus = Constant.make ModPath.initial (Label.of_string "minus")

let int_bop_type =
  let x = Name.mk_name @@ Id.of_string "x" in
  let y = Name.mk_name @@ Id.of_string "y" in
  Constr.mkProd (x, int_type, Constr.mkProd (y, int_type, int_type))

let basic_env =
  let env = MiniCic.Env.empty_env in
  env
  |> MiniCic.Env.add_constant c_plus None int_bop_type
  |> MiniCic.Env.add_constant c_minus None int_bop_type



