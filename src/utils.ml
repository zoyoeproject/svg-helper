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

let on_click_set item call_back =
  Document.add_event_listener item "click" call_back

let on_mousedown_set item call_back =
  Document.add_event_listener item "mousedown" call_back

let on_mouseup_set item call_back =
  Document.add_event_listener item "mouseup" call_back

let mk_use svgname (x, y) =
  Printf.sprintf "<use href='#%s' x='%d' y='%d' width='30' height='30'/>" svgname x y



