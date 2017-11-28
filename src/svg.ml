open Vec2
open Webapi.Dom

(** SVG DOM object type *)
type t = Element.t

let getAttr name elem =
  Element.getAttribute name elem

let setAttr name value elem =
  Element.setAttribute name value elem

(** Apply function to the attribute and change it *)
let changeAttr name valueTranslator elem =
  let prev = getAttr name elem in
  match Option.map valueTranslator prev with
  | Some(next) -> Element.setAttribute name next elem
  | None -> ()

let deleteAttr name elem =
  Element.setAttribute name [%bs.raw {| null |}] elem

(** Move element parallel *)
let move (delta: Vec2.t) elem =
  let tx = fun prev ->
    (float_of_string prev) +. delta.x
    |> string_of_float
  in
  let ty prev =
    (float_of_string prev) +. delta.y
    |> string_of_float
  in
  match Element.tagName elem with
  | "circle" | "ellipse" ->
    changeAttr "cx" tx elem;
    changeAttr "cy" ty elem
  | "image" | "text" | "rect" | "use" ->
    changeAttr "x" tx elem;
    changeAttr "y" ty elem
  | "line" ->
    changeAttr "x1" tx elem;
    changeAttr "y1" ty elem;
    changeAttr "x2" tx elem;
    changeAttr "y2" ty elem
  | "polygon" | "polyline" ->
    let points = getAttr "points" elem
    |> Option.map Parsers.parsePoints
    |> Option.default []
    |> List.map (fun p -> p +^ delta)
    in
    setAttr "points" (Parsers.genPoints points) elem
  | "path" ->
    let d = getAttr "d" elem
    |> Option.map Parsers.parseD
    |> Option.default []
    |> List.map (fun op -> Path.{op with points =
      (List.map (fun p -> p +^ delta) op.points)
    })
    in
    setAttr "d" (Parsers.genD d) elem
  | _ ->
    ()

(** target element -> Vec2.t
Get left top coordinate of the element *)
let getLeftTop elem  =
  let box = Box.getBBox elem  in
  box.leftTop

(** Vec2.t -> target element -> unit
Set left top coordinate of the element *)
let setLeftTop vec2 elem  =
  let prev= getLeftTop elem  in
  let delta = vec2 -^ prev in
  move delta elem 

let getRightBottom elem  =
  let box = Box.getBBox elem  in
  box.rightBottom

let getCenter elem  =
  let box = Box.getBBox elem  in
  ((box.leftTop +^ box.rightBottom) /^ {x=2.0;y=2.0})

let setCenter vec2 elem  =
  let delta = vec2 -^ (getCenter elem ) in
  move delta elem 

(** zoom ratio -> svg element -> target element -> unit
Zoom the element *)
let zoom (ratio: Vec2.t) elem  =
  let center = getCenter elem  in
  let mulK name k =
    changeAttr name
    (
      fun (prevStr: string) ->
      let prev = float_of_string prevStr in
      string_of_float (prev *. k)
    ) elem  in
  match Element.tagName elem  with
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
    let points = getAttr "points" elem 
    |> Option.map Parsers.parsePoints
    |> Option.default []
    |> List.map (fun p -> p *^ ratio)
    in
    setAttr "points" (Parsers.genPoints points) elem 
  | "path" ->
    let d = getAttr "d" elem 
    |> Option.map Parsers.parseD
    |> Option.default []
    |> List.map (fun op -> Path.{op with points =
      (List.map (fun p -> p *^ ratio) op.points)
    })
    in
    setAttr "d" (Parsers.genD d) elem 
  | _ ->
    ()
  ;
  setCenter center elem 

(** Get the element size, it is calculated by bounding client rect *)
let getSize elem  =
  let box = Box.getBBox elem  in
  box.rightBottom -^ box.leftTop

(** Set the element size *)
let setSize vec2 elem  =
  zoom (vec2 /^ (getSize elem )) elem 

(** target element -> Color.t
Get fill computed color of the target, includes alpha *)
let getFillColor elem  =
  let style = Natives.getComputedStyle elem  in
  if style##fill == "" then Color.None
  else
    match Parsers.parseRgb @@ style##fill with
    | Color.Rgb rgb -> Color.Rgba {r = rgb.r; g = rgb.g; b = rgb.b; a = float_of_string style##fillOpacity}
    | _ -> Color.None

(** Color.t -> target elememt -> unit
Set fill color of the target, includes alpha *)
let setFillColor (color: Color.t) elem  =
  let style = Natives.getStyle elem  in
  match color with
  | Color.None -> style##fill #= [%bs.raw {|null|}]; style##fillOpacity #= [%bs.raw {|null|}]
  | Color.Rgb _ -> style##fill #= (Parsers.genColor color)
  | Color.Rgba rgba -> style##fill #= (Parsers.genColor color); style##fillOpacity #= (string_of_float rgba.a)

(** target element -> Color.t
Get stroke computed color of the target, includes alpha *)
let getStrokeColor elem  =
  let style = Natives.getComputedStyle elem  in
  if style##stroke == "" then Color.None
  else
    match Parsers.parseRgb @@ style##stroke with
    | Color.Rgb rgb -> Color.Rgba {r = rgb.r; g = rgb.g; b = rgb.b; a = float_of_string style##strokeOpacity}
    | _ -> Color.None

(** Color.t -> target elememt -> unit
Set stroke color of the target, includes alpha *)
let setStrokeColor (color: Color.t) elem  =
  let style = Natives.getStyle elem  in
  match color with
  | Color.None -> style##stroke #= [%bs.raw {|null|}]; style##strokeOpacity #= [%bs.raw {|null|}]
  | Color.Rgb _ -> style##stroke #= (Parsers.genColor color)
  | Color.Rgba rgba -> style##strokeOpacity #= (string_of_float rgba.a)

(** Get the style object *)
let getStyle = Natives.getStyle
