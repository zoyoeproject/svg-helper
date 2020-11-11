let set_name item name =
  match name with
  | None -> ()
  | Some n -> Document.setAttribute item "id" n

let mk_text style (x,y) txt =
  Printf.sprintf "<text x='%d' y='%d' class='%s'>%s</text>" x y style txt

let mk_group_in parent name svg =
  let item = Document.createElementSVG Document.document "g" in
  set_name item name;
  Document.setInnerHTML item svg;
  Document.appendChild parent item;
  item

let set_translate_matrix parent item (x, y) =
  let transform = Document.transform item in
  let base_transforms = transform.baseVal in
  let matrix = Document.createSVGMatrix parent in
  let matrix = Document.translate matrix x y in
  let matrix_transform = Document.createTransform base_transforms matrix in
  Document.appendItem base_transforms matrix_transform

let mk_use svgname (x, y) =
  Printf.sprintf "<use href='#%s' x='%d' y='%d' width='30' height='30'/>" svgname x y

let on_click_set item call_back =
  Document.add_event_listener item "click" call_back

let on_mousedown_set item call_back =
  Document.add_event_listener item "mousedown" call_back

let on_mouseup_set item call_back =
  Document.add_event_listener item "mouseup" call_back

let on_mousemove_set item call_back =
  Document.add_event_listener item "mousemove" call_back

type dd_info = {
  item: Document.element;
  cb: Document.element-> unit;
}

let init_dragdrop_item parent item callback context =

  let handle_mouse_down _ =
    context := Some {item = item; cb = callback}
  in

  let handle_mouse_up _ = () in

  on_mousedown_set item handle_mouse_down;
  on_mouseup_set item handle_mouse_up

let get_translate_info i =
  let transform = Document.transform i in
  let transform = transform.baseVal.(0) in
  let matrix = Document.getMatrix transform in
  Document.(matrix.e, matrix.f)

let init_dragdrop item =
  let pan_state = ref Event.Nothing in
  let transform = Document.transform item in
  let base_transforms = transform.baseVal in
  let matrix = Document.createSVGMatrix item in
  let matrix_transform = Document.createTransform base_transforms matrix in
  Document.appendItem base_transforms matrix_transform;
  Js.log @@ Array.length transform.baseVal;

  let context = ref None in

  let dragdrop (px, py) (cx, cy) cont =
    let i = match !context with
      | None -> Js.log "none"; item
      | Some i -> i.item
    in
    let transform = Document.transform i in
    let transform = transform.baseVal.(0) in
    let matrix = Document.getMatrix transform in
    let t = cx - px, cy - py in
    let matrix = Document.translate matrix (fst t) (snd t) in
    Document.setMatrix transform matrix;
    let _ = match !context with
      | Some i -> i.cb i.item
      | _ -> ()
    in
    if cont then context := None
  in

  let handle_mouse_down minfo =
    pan_state := Event.pan_handler dragdrop !pan_state (Event.Down Document.(minfo.clientX, minfo.clientY))
  in

  let handle_mouse_up minfo =
    pan_state := Event.pan_handler dragdrop !pan_state (Event.Up Document.(minfo.clientX, minfo.clientY))
  in

  let handle_mouse_move minfo =
    pan_state := Event.pan_handler dragdrop !pan_state (Event.Move Document.(minfo.clientX, minfo.clientY))
  in

  on_mousedown_set item handle_mouse_down;
  on_mouseup_set item handle_mouse_up;
  on_mousemove_set item handle_mouse_move;
  context

