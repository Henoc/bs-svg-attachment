type domRect = {
  x: float;
  y: float;
  width: float;
  height: float;
  top: float;
  right: float;
  bottom: float;
  left: float;
}

type svgElement = {
  getAttribute: string -> string Js.nullable;
  setAttribute: string -> string -> unit;
  getBoundingClientRect: unit -> domRect;
}
