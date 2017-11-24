open Vec2
open Webapi.Dom
open Natives

type t = {
  rootElem: Element.t;
  elem: Element.t;
}

let getAttr t name =
  Element.getAttribute name t.elem

let setAttr t name value =
  Element.setAttribute name value t

let changeAttr t name valueTranslator =
  let prev = getAttr t name in
  match Option.map valueTranslator prev with
  | Some(next) -> Element.setAttribute name next t.elem
  | None -> ()

let deleteAttr t name =
  Element.setAttribute name [%bs.raw {| null |}] t.elem

let move t (delta: Vec2.t) =
  let tx = fun prev ->
    (float_of_string prev) +. delta.x
    |> string_of_float
  in
  let ty prev =
    (float_of_string prev) +. delta.y
    |> string_of_float
  in
  match Element.tagName t.elem with
  | "circle" | "ellipse" ->
    changeAttr t "cx" tx;
    changeAttr t "cy" ty
  | "image" | "text" | "rect" | "use" ->
    changeAttr t "x" tx;
    changeAttr t "y" ty
  | "line" ->
    changeAttr t "x1" tx;
    changeAttr t "y1" ty;
    changeAttr t "x2" tx;
    changeAttr t "y2" ty
  | "polygon" | "polyline" ->
    ()
  | _ ->
    ()


let getBBox t =
  Natives.getBoundingClientRect t.elem

let getRootLeftTop: t -> Vec2.t = fun t ->
  let rootBox = Natives.getBoundingClientRect t.rootElem in
  {x = rootBox.left; y = rootBox.top}

let getRootRightBottom: t -> Vec2.t = fun t ->
  let rootBox = Natives.getBoundingClientRect t.rootElem in
  {x = rootBox.right; y = rootBox.bottom}

let getRootCenter: t -> Vec2.t = fun t ->
  let rootBox = Natives.getBoundingClientRect t.rootElem in
  {x = (rootBox.left +. rootBox.right) /. 2.0;
  y = (rootBox.top +. rootBox.bottom) /. 2.0}

let getLeftTop t =
  let box = getBBox t in
  let ground = getRootLeftTop t in
  {x=box.left; y=box.top} -^ ground

let setLeftTop t vec2 =
  let prev= getLeftTop t in
  let delta = vec2 -^ prev in
  move t delta

let getRightBottom t =
  let box = getBBox t in
  let ground = getRootRightBottom t in
  {x=box.right; y=box.bottom} -^ ground

let getCenter t =
  let box = getBBox t in
  let ground = getRootCenter t in
  {
    x = (box.left +. box.right) /. 2.0;
    y = (box.top +. box.bottom) /. 2.0
  } -^ ground

let setCenter t vec2 =
  let delta = vec2 -^ (getCenter t) in
  move t delta

let zoom t (ratio: Vec2.t) =
  let center = getCenter t in
  let mulK name k =
    changeAttr t name
    (
      fun (prevStr: string) ->
      let prev = Js.Float.fromString prevStr in
      Js.Float.toString (prev *. k)
    ) in
  match Element.tagName t.elem with
  | "circle" -> mulK "r" ratio.x
  | "ellipse" ->
    mulK "rx" ratio.x;
    mulK "ry" ratio.y
  | "text" ->
    mulK "font-size" ratio.x
  | "rect" | "use" ->
    mulK "width" ratio.x;
    mulK "height" ratio.y
  | "line" ->
    mulK "x2" ratio.x;
    mulK "y2" ratio.y
  | "polygon" | "polyline" -> 
    ()
  | "path" ->
    ()
  | _ ->
    ()
  ;
  setCenter t center

let getSize t =
  let box = getBBox t in
  {x = box.width; y = box.height}

let setSize t vec2 =
  zoom t @@ vec2 /^ (getSize t)

let getFillColor t =
  let style = Window.getComputedStyle t.elem DomRe.window in
  ()