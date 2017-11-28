open Webapi.Dom
open Vec2

type t = Element.t list

let makeBox elems =
  let rec loop elems acc = match elems with
  | hd :: tl ->
    Box.getBBox hd
    |> Box.merge acc
    |> loop tl
  | [] -> acc
  in
  match elems with
  | hd :: tl -> loop tl @@ Box.getBBox hd
  | [] -> raise Not_found

let getLeftTop elems =
  let box = makeBox elems in
  box.leftTop

let setLeftTop vec2 elems =
  let delta = vec2 -^ (getLeftTop elems) in
  List.map (fun e -> Svg.move delta e) elems
  |> ignore

let getRightBottom elems =
  let box = makeBox elems in
  box.rightBottom

let setRightBottom vec2 elems =
  let delta = vec2 -^ (getRightBottom elems) in
  List.map (fun e -> Svg.move delta e) elems
  |> ignore

let getCenter elems =
  let box = makeBox elems in
  ((box.leftTop +^ box.rightBottom) /^ {x=2.0;y=2.0})

let setCenter vec2 elems =
  let delta = vec2 -^ (getCenter elems) in
  List.map (fun e -> Svg.move delta e) elems
  |> ignore

let zoom ratio elems =
  let center = getCenter elems in
  elems
  |> List.map @@ fun e ->
    Svg.zoom ratio e;
    let v = Svg.getCenter e in
    Svg.setCenter
      ((v *^ ratio) +^ (({x=1.0;y=1.0} -^ ratio) *^ center))
      e
  |> ignore

let getSize elems =
  let box = makeBox elems in
  box.rightBottom -^ box.leftTop

let setSize vec2 elems =
  let ratio = vec2 /^ (getSize elems) in
  zoom ratio elems

