type focus =
  | Connect of (Document.element * (Node.var * Constr.t))
  | Create of (Document.element * (Names.Constant.t * Constr.t))

type context_info = {
  dragdrop: (Document.element * (Document.element -> unit)) option;
  focus: focus option;
  cfg_ele: Document.element;
}
