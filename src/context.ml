module NodeMap = Map.Make (String)
module Names = MiniCic.Names
module Constr = MiniCic.Constr

type node = Node.t DagreFFI.node_size

type node_map = node NodeMap.t

type constr_encoder = string array -> Constr.t

type prompt_info = {label: string; info: string; default: string}

type creator = CreatorNode of Constr.t | CreatorVar

type focus =
  | Connect of Document.element * (Node.var * Constr.t)
  | Create of Document.element * creator

type context_info =
  { mutable ssa_count: int
  ; mutable dragdrop: (Document.element * (Document.element -> unit)) option
  ; mutable focus: focus option
  ; mutable cfg_ele: Document.element
  ; mutable cfg_param_ele: Document.element
  ; mutable nodes: Node.t DagreFFI.node_size NodeMap.t
  ; mutable prompt:
      string -> prompt_info array -> (string array -> unit) -> unit
  ; (* title -> item_description -> handler *)
    mutable env: MiniCic.Env.env
  ; parse_expr: string -> MiniCic.Env.env -> MiniCic.Constr.t
  ; parse_type: string -> MiniCic.Env.env -> MiniCic.Constr.t }

let new_ssa ctxt =
  ctxt.ssa_count <- ctxt.ssa_count + 1 ;
  "ssa" ^ string_of_int ctxt.ssa_count

let get_focus_element = function
  | Create (focus, _) -> focus
  | Connect (focus, _) -> focus

let option_map f = function Some a -> Some (f a) | None -> None

let add_class style x : string = String.concat " " [x; style]

let remove_calss style x : string =
  let styles = String.split_on_char ' ' style in
  let styles = List.filter (fun t -> t <> x && t <> "") styles in
  String.concat " " styles

let toggle_focus context focus =
  let fele = get_focus_element focus in
  match option_map get_focus_element context.focus with
  | Some f ->
      let style = Document.getAttribute f "class" in
      let style = remove_calss style "focus" in
      Document.setAttribute f "class" style ;
      if f == fele then (
        context.focus <- None ;
        false )
      else
        let style = Document.getAttribute fele "class" in
        let style = add_class style "focus" in
        Document.setAttribute fele "class" style ;
        context.focus <- Some focus ;
        true
  | _ ->
      context.focus <- Some focus ;
      let style = Document.getAttribute fele "class" in
      let style = add_class style "focus" in
      Document.setAttribute fele "class" style ;
      true

let clear_focus context =
  let _ =
    match option_map get_focus_element context.focus with
    | Some f ->
        let style = Document.getAttribute f "class" in
        let style = remove_calss style "focus" in
        Document.setAttribute f "class" style
    | _ -> ()
  in
  context.focus <- None

let get_focus_connect context =
  match context.focus with Some (Connect (_, var)) -> Some var | _ -> None

let get_focus_create context =
  match context.focus with Some (Create (_, var)) -> Some var | _ -> None

let build_edges graph nodes =
  let open Node in
  NodeMap.iter
    (fun name node ->
      let node = DagreFFI.extract node in
      let src = name in
      Array.iter
        (fun param ->
          match param.input with
          | Some (PATH (node_name, _, _)) ->
              DagreFFI.add_edge graph node_name src
          | _ -> () )
        node.inputs )
    nodes

let init_layout graph nodes =
  NodeMap.iter
    (fun node_name node -> DagreFFI.add_node graph node_name node)
    nodes ;
  build_edges graph nodes ;
  DagreFFI.layout graph

let global_ctxt = ref None

let get_global_context () =
  match !global_ctxt with
  | None ->
      Js.log "global context hasn't been init" ;
      raise (Exceptions.CFG_ERROR "CFG has not been initialized")
  | Some ctxt -> ctxt

let init_context prompt env parse_type parse_expr param_div svg_div nodes =
  let graph = DagreFFI.create_graph () in
  init_layout graph nodes ;
  global_ctxt :=
    Some
      { ssa_count= 0
      ; cfg_ele= svg_div
      ; cfg_param_ele= param_div
      ; dragdrop= None
      ; focus= None
      ; nodes
      ; prompt
      ; env
      ; parse_type
      ; parse_expr } ;
  get_global_context ()

(* Iterate the nodes as topo-sort *)
let node_map_sorted_array node_map =
  let open Node in
  let open TopoSort in
  let f_get_deps graph_node =
    let node = DagreFFI.extract graph_node in
    Array.fold_left
      (fun dep param ->
        match param.input with
        | Some (PATH (in_node_name, _, _)) -> NodeMap.add in_node_name true dep
        | _ -> dep )
      NodeMap.empty node.inputs
  in
  let elements =
    NodeMap.bindings node_map
    |> List.map (fun (x, y) -> (x, f_get_deps y))
    |> Array.of_list
  in
  let f_dec_deps = NodeMap.remove in
  let f_cardinal_deps = NodeMap.cardinal in
  topo_sort f_dec_deps f_cardinal_deps elements

let node_map_sorted_iter f node_map =
  node_map_sorted_array node_map
  |> Array.iter (fun name -> f name (NodeMap.find name node_map))

let node_map_sorted_fold f init node_map =
  node_map_sorted_array node_map
  |> Array.fold_left
       (fun init name -> f init name (NodeMap.find name node_map))
       init
