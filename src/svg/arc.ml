let mk_line style (x1, y1) (x2, y2) =
  Printf.sprintf "<line x1='%f' y1='%f' x2='%f' y2='%f' class='%s' />" x1 y1 x2
    y2 style

let mk_arc_curve style (x1, y1) (x2, y2) (cx, cy) =
  Printf.sprintf "<path d='M %f %f Q %f %f, %f %f' class='%s'/>" x1 y1 cx cy x2
    y2 style

let mk_bezier_curve style s e ctrl_start ctrl_end =
  let sx, sy = s in
  let ex, ey = e in
  let csx, csy = ctrl_start in
  let cex, cey = ctrl_end in
  Printf.sprintf "<path d='M %f %f C %f %f, %f %f, %f %f' class='%s'/>" sx sy
    csx csy cex cey ex ey style

(* connection of two points with two bezier curves
 * for the purpose of connecting two points that are spread horizontal
 *   start --> ctrl_start     ctrl_mid
 *                               |\
 *                               |
 *                               mid
 *                               |
 *                               |/
 *                            ctrl_mid     ctrl_end <-- end
 *)
let connect_horizontal style s e =
  let sx, sy = s in
  let ex, ey = e in
  let mx, my = ((sx +. ex) /. 2., (sy +. ey) /. 2.) in
  let csx, csy = (mx, sy) in
  let cmx, cmy = (mx, sy) in
  let cex, cey = (mx, ey) in
  Printf.sprintf
    "<path d='M %f %f C %f %f, %f %f, %f %f S %f %f, %f %f' class='%s'/>" sx sy
    csx csy cmx cmy mx my cex cey ex ey style

(* connection of two points with two bezier curves
 * for the purpose of connecting two points that are spread vertical
 *   start
 *     |
 *     |/
 *  ctrl_start --->  ctrl_mid
 *                   |\
 *                   |
 *                   mid
 *                   |
 *                   |/
 *                   ctrl_mid  --->  ctrl_end
 *                                     |\
 *                                     |
 *                                    end
 *)
let connect_vertical style s e =
  let sx, sy = s in
  let ex, ey = e in
  let mx, my = ((sx +. ex) /. 2., (sy +. ey) /. 2.) in
  let csx, csy = (sx, my) in
  let cmx, cmy = (mx, sy) in
  let cex, cey = (ex, my) in
  Printf.sprintf
    "<path d='M %f %f C %f %f, %f %f, %f %f S %f %f, %f %f' class='%s'/>" sx sy
    csx csy cmx cmy mx my cex cey ex ey style
