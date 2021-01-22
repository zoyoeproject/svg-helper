
let get_demo_env () =
  let open MiniCic.CoreType in
  let open Global in
  let open MiniCic.Names in
  let open MiniCic.Constr in
  let env = Global.basic_env in
  let x = Id.of_string "x" in
  let y = Id.of_string "y" in
  let z = Id.of_string "z" in
  let cond = Id.of_string "cond" in
  let app = mkApp (mkConstU (c_plus, 1), [|Constr.mkVar x; Constr.mkVar y|]) in
  let app = mkApp (mkConstU (c_plus, 1), [|Constr.mkVar z; app|]) in
  let result = mkCase (bool_case_info env, int_type, mkVar cond, [|app; mkVar x|]) in
  let n = Constr.mkApp (mkConstU (c_minus, 1), [|result; Int 2|]) in
  let r = Id.of_string "r" in
  let n' = Id.of_string "n" in
  let env = env
    |> MiniCic.Env.push_named (LocalAssum (x, int_type))
    |> MiniCic.Env.push_named (LocalAssum (y, int_type))
    |> MiniCic.Env.push_named (LocalAssum (z, int_type))
    |> MiniCic.Env.push_named (LocalDef (r, app, int_type))
    |> MiniCic.Env.push_named (LocalDef (n', n, int_type))
    |> MiniCic.Env.push_named (LocalAssum (cond, bool_type))
    |> MiniCic.Env.export n'
  in env

let display_env_as_cfg prompt tool_div parent env =
  ignore @@ CfgEditor.build_cfg prompt tool_div parent env
