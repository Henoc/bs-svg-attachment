open Vec2
open Webapi.Dom

type t = {
  rootElem: Element.t;
  elem: Element.t;
}

let getAttr name t =
  Element.getAttribute name t.elem

let setAttr name value t =
  Element.setAttribute name value t.elem

let changeAttr name valueTranslator t =
  let prev = getAttr name t in
  match Option.map valueTranslator prev with
  | Some(next) -> Element.setAttribute name next t.elem
  | None -> ()

let deleteAttr name t =
  Element.setAttribute name [%bs.raw {| null |}] t.elem

let move (delta: Vec2.t) t =
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
    changeAttr "cx" tx t;
    changeAttr "cy" ty t
  | "image" | "text" | "rect" | "use" ->
    changeAttr "x" tx t;
    changeAttr "y" ty t
  | "line" ->
    changeAttr "x1" tx t;
    changeAttr "y1" ty t;
    changeAttr "x2" tx t;
    changeAttr "y2" ty t
  | "polygon" | "polyline" ->
    let points = getAttr "points" t
    |> Option.map Parsers.parsePoints
    |> Option.default []
    |> List.map (fun p -> p +^ delta)
    in
    setAttr "points" (Parsers.genPoints points) t
  | "path" ->
    let d = getAttr "d" t
    |> Option.map Parsers.parseD
    |> Option.default []
    |> List.map (fun op -> Path.{op with points =
      (List.map (fun p -> p +^ delta) op.points)
    })
    in
    setAttr "d" (Parsers.genD d) t
  | _ ->
    ()


let getBBox t =
  Natives.getBoundingClientRect t.elem

let getRootLeftTop: t -> Vec2.t = fun t ->
  let rootBox = Natives.getBoundingClientRect t.rootElem in
  {x = rootBox##left; y = rootBox##top}

let getRootRightBottom: t -> Vec2.t = fun t ->
  let rootBox = Natives.getBoundingClientRect t.rootElem in
  {x = rootBox##right; y = rootBox##bottom}

let getRootCenter: t -> Vec2.t = fun t ->
  let rootBox = Natives.getBoundingClientRect t.rootElem in
  {x = (rootBox##left +. rootBox##right) /. 2.0;
  y = (rootBox##top +. rootBox##bottom) /. 2.0}

let getLeftTop t =
  let box = getBBox t in
  let ground = getRootLeftTop t in
  {x=box##left; y=box##top} -^ ground

let setLeftTop vec2 t =
  let prev= getLeftTop t in
  let delta = vec2 -^ prev in
  move delta t

let getRightBottom t =
  let box = getBBox t in
  let ground = getRootLeftTop t in
  {x=box##right; y=box##bottom} -^ ground

let getCenter t =
  let box = getBBox t in
  let ground = getRootLeftTop t in
  {
    x = (box##left +. box##right) /. 2.0;
    y = (box##top +. box##bottom) /. 2.0
  } -^ ground

let setCenter vec2 t =
  let delta = vec2 -^ (getCenter t) in
  move delta t

let zoom (ratio: Vec2.t) t =
  let center = getCenter t in
  let mulK name k =
    changeAttr name
    (
      fun (prevStr: string) ->
      let prev = float_of_string prevStr in
      string_of_float (prev *. k)
    ) t in
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
    let points = getAttr "points" t
    |> Option.map Parsers.parsePoints
    |> Option.default []
    |> List.map (fun p -> p *^ ratio)
    in
    setAttr "points" (Parsers.genPoints points) t
  | "path" ->
    let d = getAttr "d" t
    |> Option.map Parsers.parseD
    |> Option.default []
    |> List.map (fun op -> Path.{op with points =
      (List.map (fun p -> p *^ ratio) op.points)
    })
    in
    setAttr "d" (Parsers.genD d) t
  | _ ->
    ()
  ;
  setCenter center t

let getSize t =
  let box = getBBox t in
  {x = box##width; y = box##height}

let setSize vec2 t =
  zoom (vec2 /^ (getSize t)) t

let getFillColor t =
  let style = Natives.getComputedStyle t.elem in
  if style##fill == "" then Color.None
  else
    match Parsers.parseRgb @@ style##fill with
    | Color.Rgb rgb -> Color.Rgba {r = rgb.r; g = rgb.g; b = rgb.b; a = float_of_string style##fillOpacity}
    | _ -> Color.None

let setFillColor (color: Color.t) t =
  let style = Natives.getStyle t.elem in
  match color with
  | Color.None -> style##fill #= [%bs.raw {|null|}]; style##fillOpacity #= [%bs.raw {|null|}]
  | Color.Rgb _ -> style##fill #= (Parsers.genColor color)
  | Color.Rgba rgba -> style##fill #= (Parsers.genColor color); style##fillOpacity #= (string_of_float rgba.a)

let getStrokeColor t =
  let style = Natives.getComputedStyle t.elem in
  if style##stroke == "" then Color.None
  else
    match Parsers.parseRgb @@ style##stroke with
    | Color.Rgb rgb -> Color.Rgba {r = rgb.r; g = rgb.g; b = rgb.b; a = float_of_string style##strokeOpacity}
    | _ -> Color.None

let setStrokeColor (color: Color.t) t =
  let style = Natives.getStyle t.elem in
  match color with
  | Color.None -> style##stroke #= [%bs.raw {|null|}]; style##strokeOpacity #= [%bs.raw {|null|}]
  | Color.Rgb _ -> style##stroke #= (Parsers.genColor color)
  | Color.Rgba rgba -> style##strokeOpacity #= (string_of_float rgba.a)

let getStyle = Natives.getStyle
