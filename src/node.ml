open MiniCic

type var =
  | VAR of Constr.t
  | PATH of string * Names.Name.t (* node name, ret name *)

type param = {
  para_info: (string * Constr.t); (* Name, Type *)
  mutable input: var option;
}

type t = {
  name: string;
  mutable src: Constr.t;
  inputs: param array;
  outputs: (Names.Name.t * Constr.t) array;
}

let mk_path a b = PATH (a,b)
let mk_var a = VAR a
let mk_param info input = {para_info = info; input = input}

let mk_node name src inputs outputs = {name=name; src=src; inputs=inputs; outputs=outputs}

let default_node_height = 40

let total_sz n = (n+1)*10

let padding_sz l n = (l - (total_sz n)) / 2

let compute_size node =
  match node.src with
  | Var _ -> (15,15)
  | _ -> let nb_eles = max (Array.length node.inputs) (Array.length node.outputs) in
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
    | h :: t -> if x = fst h then 0 else 1 + find_idx t
  in
  find_idx (Array.to_list node.outputs)