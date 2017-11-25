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
  clipPath: string [@bs.set];
  clipRule: string [@bs.set];
  mask: string [@bs.set];
  opacity: string [@bs.set];
  enableBackground: string [@bs.set];
  filter: string [@bs.set];
  floodColor: string [@bs.set];
  floodOpacity: string [@bs.set];
  lightingColor: string [@bs.set];
  stopColor: string [@bs.set];
  stopOpacity: string [@bs.set];
  pointerEvents: string [@bs.set];
  colorInterpolation: string [@bs.set];
  colorInterpolationFilters: string [@bs.set];
  colorProfile: string [@bs.set];
  colorRendering: string [@bs.set];
  fill: string [@bs.set];
  fillOpacity: string [@bs.set];
  fillRule: string [@bs.set];
  imageRendering: string [@bs.set];
  marker: string [@bs.set];
  markerEnd: string [@bs.set];
  markerMid: string [@bs.set];
  markerStart: string [@bs.set];
  shapeRendering: string [@bs.set];
  stroke: string [@bs.set];
  strokeDasharray: string [@bs.set];
  strokeDashoffset: string [@bs.set];
  strokeLinecap: string [@bs.set];
  strokeLinejoin: string [@bs.set];
  strokeMiterlimit: string [@bs.set];
  strokeOpacity: string [@bs.set];
  strokeWidth: string [@bs.set];
  textRendering: string [@bs.set];
  alignmentBaseline: string [@bs.set];
  baselineShift: string [@bs.set];
  dominantBaseline: string [@bs.set];
  glyphOrientationHorizontal: string [@bs.set];
  glyphOrientationVertical: string [@bs.set];
  kerning: string [@bs.set];
  textAnchor: string [@bs.set];
  writingMode: string [@bs.set];
> Js.t

external getStyle: Element.t -> svgStyle = "style" [@@bs.get]
external getComputedStyle: Element.t -> svgStyle = "" [@@bs.val] [@@bs.scope "window"]
