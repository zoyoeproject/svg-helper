module NodeMap = Map.Make (String)
module Names = MiniCic.Names
module Constr = MiniCic.Constr

type node_map = (Node.t DagreFFI.node_size) NodeMap.t
type node = Node.t DagreFFI.node_size

type focus =
  | Connect of (Document.element * (Node.var * Constr.t))
  | Create of (Document.element * (Names.Constant.t * Constr.t))

type context_info = {
  mutable ssa_count: int;
  mutable dragdrop: (Document.element * (Document.element -> unit)) option;
  mutable focus: focus option;
  mutable cfg_ele: Document.element;
  mutable nodes: (Node.t DagreFFI.node_size) NodeMap.t;
}

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
