(* elements is (ele, deps) array *)
let topo_sort f_dec_deps f_cardinal_deps elements =
  let len = Array.length elements in
  let elements = Array.map (fun (e, deps) -> (e, deps, len)) elements in
  let f_sort (_, d1, p1) (_, d2, p2) =
    if p1 <> p2 then p1 - p2 else f_cardinal_deps d1 - f_cardinal_deps d2
  in
  let rec aux idx =
    if idx <> len then (
      Array.fast_sort f_sort elements ;
      let curr, curr_d, _ = elements.(idx) in
      if f_cardinal_deps curr_d > 0 then (
        Js.log "circle detected in topo_sort" ;
        assert false )
      else (
        elements.(idx) <- (curr, curr_d, idx) ;
        Array.iteri
          (fun i (e, deps, prio) ->
            if prio = len then elements.(i) <- (e, f_dec_deps curr deps, prio)
            )
          elements ;
        aux (idx + 1) ) )
  in
  aux 0 ;
  Array.map (fun (x, _, _) -> x) elements
