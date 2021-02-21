type ffi_graph

type graphlib

type graph_config =
  {rankdir: string; width: int; height: int; marginx: int; marginy: int}

type node_name = string

type 'a node_size =
  { label: string
  ; width: int
  ; height: int
  ; mutable x: int
  ; mutable y: int
  ; extra: 'a }

let extract c = c.extra

let mk_node_info label i width height =
  {label; width; height; extra= i; x= 0; y= 0}

let empty = {width= 100; height= 100; rankdir= "LR"; marginx= 40; marginy= 40}

type edge_label = {name: string}

type edge_cb = unit -> edge_label

external init : ffi_graph -> graph_config -> unit = "setGraph" [@@bs.send]

external set_edge_label : ffi_graph -> edge_cb -> unit = "setDefaultEdgeLabel"
  [@@bs.send]

external nodes : ffi_graph -> string array = "nodes" [@@bs.send]

external add_node : ffi_graph -> node_name -> 'a node_size -> unit = "setNode"
  [@@bs.send]

external get_node : ffi_graph -> node_name -> 'a node_size = "node" [@@bs.send]

external add_edge : ffi_graph -> node_name -> node_name -> unit = "setEdge"
  [@@bs.send]

external graphlib : graphlib = "graphlib" [@@bs.val] [@@bs.module "dagre"]

external layout : ffi_graph -> unit = "layout" [@@bs.val] [@@bs.module "dagre"]

external create : unit -> ffi_graph = "Dagre.graphlib.Graph" [@@bs.new]

let create_graph _ =
  let _ = graphlib in
  let graph = create () in
  init graph empty ;
  set_edge_label graph (fun _ -> {name= ""}) ;
  graph
