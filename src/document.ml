type dom
type element
type daoism
type style

external document : dom = "document" [@@bs.val]
external get_by_id: dom -> string -> element = "getElementById" [@@bs.send]
external add_event_listener: element -> string -> 'a -> unit = "addEventListener" [@@bs.send]
external innerHTML : element -> string = "innerHTML" [@@bs.get]
external outerHTML : element -> string = "innerHTML" [@@bs.get]
external style : element -> style = "style" [@@bs.get]

external setCursor : style -> string -> unit = "cursor" [@@bs.set]

external setInnerHTML : element -> string -> unit = "innerHTML" [@@bs.set]
external setOuterHTML : element -> string -> unit = "outerHTML" [@@bs.set]
external setAttribute : element -> string -> string -> unit = "" [@@bs.send]

external createElement : dom -> string -> element
    = "createElement" [@@bs.send]
external createElementNS : dom -> string -> string -> element
    = "createElementNS" [@@bs.send]

external appendChild: element -> element -> unit = "appendChild" [@@bs.send]

let svg_ns = "http://www.w3.org/2000/svg"
let createElementSVG dom string = createElementNS dom svg_ns string


type transform

type matrix = {
  a:int;
  b:int;
  c:int;
  d:int;
  e:int;
  f:int;
}

type transform_group = {
  animVal: transform array;
  baseVal: transform array;
}

external createSVGMatrix : element -> matrix = "createSVGMatrix" [@@bs.send]
external translate: matrix -> int -> int -> matrix = "translate" [@@bs.send]
external consolidate: transform array -> transform = "consolidate" [@@bs.send]
external appendItem: transform array -> transform -> unit = "appendItem" [@@bs.send]
external createTransform: transform array -> matrix -> transform = "createSVGTransformFromMatrix" [@@bs.send]

external transform: element -> transform_group =
  "" [@@bs.get]

external setMatrix : transform -> matrix -> unit = "setMatrix" [@@bs.send]

external getMatrix : transform -> matrix = "matrix" [@@bs.get]

type mouse_event_info = {
  clientX: int;
  clientY: int;
}

external stopPropagation: mouse_event_info -> unit = "" [@@bs.send]
