open Webapi.Dom

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

let getBoundingClientRect: Element.t -> domRect = [%bs.raw {| x => x.getBoundingClientRect() |}]

type svgStyle

external style: Element.t -> svgStyle = "" [@@bs.get]

external getFill: svgStyle -> string = "fill" [@@bs.get]
external setFill: svgStyle -> string -> unit = "fill" [@@bs.set]

external getStroke: svgStyle -> string = "stroke" [@@bs.get]
external setStroke: svgStyle -> string -> unit = "stroke" [@@bs.set]

