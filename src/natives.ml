open Webapi.Dom

type domRect = <
  x: float;
  y: float;
  width: float;
  height: float;
  top: float;
  right: float;
  bottom: float;
  left: float;
> Js.t

let getBoundingClientRect: Element.t -> domRect = [%bs.raw {| x => x.getBoundingClientRect() |}]

type svgStyle = <
  fill: string [@bs.set];
  stroke: string [@bs.set];
  fillOpacity: string [@bs.set];
  strokeOpacity: string [@bs.set];
> Js.t

external getStyle: Element.t -> svgStyle = "" [@@bs.get]
external getComputedStyle: Element.t -> svgStyle = "" [@@bs.val] [@@bs.scope "window"]
