type var =
  | VAR of string
  | PATH of string * string

type param = {
  name: string;
  input: var option;
}

type node = {
  name: string;
  inputs: param array;
  outputs: string array;
}

let mk_path a b = PATH (a,b)
let mk_var a = VAR a
let mk_param name input = {name = name; input = input}

let mk_node name inputs outputs = {name=name; inputs=inputs; outputs=outputs}

let default_node_height = 40

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

let find_output_idx x node: int =
  let rec find_idx lst =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if x = h then 0 else 1 + find_idx t
  in
  find_idx (Array.to_list node.outputs)

