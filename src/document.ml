type dom
type element
type daoism

external document : dom = "document" [@@bs.val]
external get_by_id: dom -> string -> element = "getElementById" [@@bs.send]
external add_event_listener: element -> string -> 'a -> unit = "addEventListener" [@@bs.send]
external innerHTML : element -> string = "" [@@bs.get]
external outerHTML : element -> string = "" [@@bs.get]

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
