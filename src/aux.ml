let get_id_of_name n =
  let open MiniCic.Names in
  match n with Name.Name id -> Id.to_string id | _ -> assert false

let collect_prod_outputs c =
  let open MiniCic.Constr in
  let open MiniCic.Names in
  MiniCic.Prod.type_list_of_tuple_type c
  |> Array.of_list
  |> Array.map (fun t ->
    match t with
    | Abstract (n, t) -> (n, t)
    | _ -> (Name.mk_name "anon", t)
  )

let collect_type_params typ =
  let rec aux acc c =
    match c with
    | MiniCic.Constr.Prod (n, t, c) ->
        let acc = (get_id_of_name n, t) :: acc in
        aux acc c
    | _ -> (acc, c)
  in
  let acc, c = aux [] typ in
  (Array.of_list (List.rev acc), c)
