
let get_demo_env () =
  let open MiniCic.CoreType in
  let open Global in
  let open MiniCic.Names in
  let open MiniCic.Constr in
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
  in env

let display_env_as_cfg tool_div parent env =
  ignore @@ CfgEditor.build_cfg tool_div parent env
