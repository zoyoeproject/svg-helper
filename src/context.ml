module NodeMap = Map.Make (String)
module Names = MiniCic.Names
module Constr = MiniCic.Constr

type node_map = (Node.t DagreFFI.node_size) NodeMap.t
type node = Node.t DagreFFI.node_size

type focus =
  | Connect of (Document.element * (Node.var * Constr.t))
  | Create of (Document.element * (Names.Constant.t * Constr.t))

type context_info = {
  mutable dragdrop: (Document.element * (Document.element -> unit)) option;
  mutable focus: focus option;
  mutable cfg_ele: Document.element;
  mutable nodes: (Node.t DagreFFI.node_size) NodeMap.t;
}

let get_focus_element = function
  | Some (Create (focus, _)) -> Some focus
  | Some (Connect (focus, _)) -> Some focus
  | None -> None

let toggle_focus context focus =
  let _ = match context.focus with
    | Some (Create (focus, _))  -> Document.setAttribute focus "class" "default"
    | Some (Connect (focus, _))  -> Document.setAttribute focus "class" "default"
    | _ -> ()
  in
  context.focus <- Some focus

let get_focus_connect context =
  match context.focus with
    | Some (Connect (_, var)) -> Some var
    | _ -> None

let get_focus_create context =
  match context.focus with
    | Some (Create (_, var)) -> Some var
    | _ -> None
