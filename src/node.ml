type var =
  | VAR of string
  | PATH of string * string

type node = {
  name: string;
  inputs: var array;
  outputs: string array;
}

let mk_path a b = PATH (a,b)
let mk_var a = VAR a

let mk_node name inputs outputs = {name=name; inputs=inputs; outputs=outputs}

let default_node_height = 30

let total_sz n = (n+1)*10

let padding_sz l n = (l - (total_sz n)) / 2

let compute_size node =
  let nb_eles = max (Array.length node.inputs) (Array.length node.outputs) in
  (30, total_sz nb_eles)

let mk_graph_node node =
  let label = node.name in
  let width, height = compute_size node in
  DagreFFI.mk_node_info label node width height

let ancher_offset x i = x + (i+1)*10

let get_ancher x w n i =
  (ancher_offset x i) + (padding_sz w n)

let get_output_ancher node i =
  let inner = DagreFFI.extract node in
  let (w,h) = compute_size inner in
  let x, y = node.x + w/2, node.y - h/2 in
  (x, get_ancher y h (Array.length inner.outputs) i)

let get_input_ancher node i =
  let inner = DagreFFI.extract node in
  let (w,h) = compute_size inner in
  let x, y = node.x - w/2, node.y - h/2 in
  (x, get_ancher y h (Array.length inner.inputs) i)

let draw_node node (cx, cy) =
  let (w,h) = compute_size node in
  let x1,y1 = cx - w/2, cy - h/2 in
  let x2,y2 = cx + w/2, cy + h/2 in
  let box = Printf.sprintf "<polygon points=\"%d,%d %d,%d %d,%d %d,%d\" class=\"default\"/>"
    x1 y1 x2 y1 x2 y2 x1 y2 in
  let inputs,_ = Array.fold_left (fun (svg, i) _ ->
    let anc = Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"3\" class=\"default\"/>"
      x1 (get_ancher y1 h (Array.length node.inputs) i) in
    (svg^anc, i + 1)
  ) ("", 0) node.inputs in
  let outputs,_ = Array.fold_left (fun (svg, i) _ ->
    let anc = Printf.sprintf "<circle cx=\"%d\" cy=\"%d\" r=\"3\" class=\"default\"/>"
      x2 (get_ancher y1 h (Array.length node.outputs) i) in
    (svg^anc, i + 1)
  ) ("", 0) node.outputs in
  box ^ outputs ^ inputs

let find_output_idx x node: int =
  let rec find_idx lst =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if x = h then 0 else 1 + find_idx t
  in
  find_idx (Array.to_list node.outputs)

