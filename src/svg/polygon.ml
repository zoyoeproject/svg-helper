let mk_rectangle style (w, h) (cx, cy) =
  let points =
    Printf.sprintf "%d,%d %d,%d %d,%d %d,%d %d,%d" cx cy (cx + w) cy (cx + w)
      (cy + h) cx (cy + h) cx cy
  in
  Printf.sprintf "<polygon class='%s' points='%s'></polygon>" style points

let mk_rectangle_in parent style (w, h) (cx, cy) =
  let points =
    Printf.sprintf "%d,%d %d,%d %d,%d %d,%d %d,%d" cx cy (cx + w) cy (cx + w)
      (cy + h) cx (cy + h) cx cy
  in
  let item = Document.createElementSVG Document.document "polygon" in
  item |. Document.setAttribute "class" style ;
  item |. Document.setAttribute "points" points ;
  Document.appendChild parent item ;
  item

let mk_polygon r n c (cx, cy) =
  let r = Js.Int.toFloat r in
  let cx = Js.Int.toFloat cx in
  let cy = Js.Int.toFloat cy in
  let delta = 2.0 *. Js.Math._PI /. Js.Int.toFloat n in
  let parray =
    Array.init n (fun i ->
        ( cx +. (r *. Js.Math.cos (Js.Int.toFloat i *. delta))
        , cy +. (r *. Js.Math.sin (Js.Int.toFloat i *. delta)) ) )
  in
  let points =
    Array.fold_left
      (fun acc (x, y) -> acc ^ Printf.sprintf " %f,%f " x y)
      "" parray
  in
  Printf.sprintf "<polygon class='%s' points='%s'></polygon>" c points

(*
 * The following should be able to implemeted by the above via
 * rotation
 *)
let mk_hexagon r style (cx, cy) =
  let cx = Js.Int.toFloat cx in
  let cy = Js.Int.toFloat cy in
  let l = Js.Int.toFloat r in
  let d = l /. 2. in
  let h = l /. 30. *. 26. in
  let points =
    ( (cx -. l, cy)
    , (cx -. d, cy -. h)
    , (cx +. d, cy -. h)
    , (cx +. l, cy)
    , (cx +. d, cy +. h)
    , (cx -. d, cy +. h) )
  in
  let (x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5) = points in
  let points : string =
    Printf.sprintf "%f,%f %f,%f %f,%f %f,%f %f,%f %f,%f %f,%f" x0 y0 x1 y1 x2
      y2 x3 y3 x4 y4 x5 y5 x0 y0
  in
  Printf.sprintf "<polygon class='%s' points='%s'></polygon>" style points
