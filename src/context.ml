module NodeMap = Map.Make (String)
module Names = MiniCic.Names
module Constr = MiniCic.Constr

type node_map = (Node.t DagreFFI.node_size) NodeMap.t
type node = Node.t DagreFFI.node_size

type constr_promise = (Constr.t -> unit) -> unit

type constr_encoder = string array -> Constr.t

type prompt_info = {
  label: string;
  info: string;
}

type focus =
  | Connect of (Document.element * (Node.var * Constr.t))
  | Create of (Document.element * (constr_promise * Constr.t))

type context_info = {
  mutable ssa_count: int;
  mutable dragdrop: (Document.element * (Document.element -> unit)) option;
  mutable focus: focus option;
  mutable cfg_ele: Document.element;
  mutable nodes: (Node.t DagreFFI.node_size) NodeMap.t;
  mutable prompt: prompt_info array -> (string array -> unit) -> unit;
}

let mk_var_promise context f =
  context.prompt [| {label="var name"; info="text"} |] (fun args ->
    f (Constr.mkVar (Names.Id.to_string args.(0)))
  )

let mk_constant_promise context c f =
  f c

let new_ssa ctxt =
  ctxt.ssa_count <- ctxt.ssa_count + 1;
  "ssa" ^ (string_of_int ctxt.ssa_count)

let get_focus_element = function
  | Create (focus, _) -> focus
  | Connect (focus, _) -> focus

let option_map f = function
  | Some a -> Some (f a)
  | None -> None

let toggle_focus context focus =
  let fele = get_focus_element focus in
  match option_map get_focus_element context.focus with
  | Some f -> begin
      Document.setAttribute f "class" "default";
      if (f == fele) then
        begin context.focus <- None; false end
      else begin
        context.focus <- Some focus;
        Document.setAttribute fele "class" "focus";
        true
      end
    end
  | _ -> begin
      context.focus <- Some focus;
      Document.setAttribute fele "class" "focus";
      true
    end

let clear_focus context =
  let _ = match option_map get_focus_element context.focus with
  | Some f -> Document.setAttribute f "class" "default";
  | _ -> ()
  in
  context.focus <- None

let get_focus_connect context =
  match context.focus with
    | Some (Connect (_, var)) -> Some var
    | _ -> None

let get_focus_create context =
  match context.focus with
    | Some (Create (_, var)) -> Some var
    | _ -> None

let build_edges graph nodes =
  let open Node in
  NodeMap.iter (fun name node ->
    let node = DagreFFI.extract node in
    let src = name in
    Array.iter (fun param ->
      match param.input with
      | Some (PATH (node_name, _)) -> DagreFFI.add_edge graph node_name src
      | _ -> ()
    ) node.inputs
  ) nodes

let init_layout graph nodes =
  NodeMap.iter (fun node_name node ->
    DagreFFI.add_node graph node_name node
  ) nodes;
  build_edges graph nodes;
  DagreFFI.layout graph

let init_context prompt parent nodes =
  let graph = DagreFFI.create_graph () in
  init_layout graph nodes;
  let context = {
    ssa_count = 0;
    cfg_ele = parent;
    dragdrop = None;
    focus = None;
    nodes = nodes;
    prompt = prompt;
  } in context
