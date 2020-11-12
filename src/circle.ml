let mk_circle_in parent style r (cx, cy) =
  let item = Document.createElementSVG Document.document "circle" in
  item |. Document.setAttribute "class" style;
  item |. Document.setAttribute "cx" (string_of_int cx);
  item |. Document.setAttribute "cy" (string_of_int cy);
  item |. Document.setAttribute "r" (string_of_int r);
  Document.appendChild parent item;
  item


