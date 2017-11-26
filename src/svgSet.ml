open Webapi.Dom
open Vec2

type t = Element.t list

let makeBox elems =
  let rec loop elems acc = match elems with
  | hd :: tl ->
    Box.getRBox hd
    |> Box.merge acc
    |> loop tl
  | [] -> acc
  in
  match elems with
  | hd :: tl -> loop tl @@ Box.getRBox hd
  | [] -> raise Not_found

let getLeftTop root elems =
  let box = makeBox elems in
  let rootBox = Box.getRBox root in
  box.leftTop -^ rootBox.leftTop

let setLeftTop vec2 root elems =
  let delta = vec2 -^ (getLeftTop root elems) in
  List.map (fun e -> Svg.move delta e) elems
  |> ignore

let getRightBottom root elems =
  let box = makeBox elems in
  let rootBox = Box.getRBox root in
  box.rightBottom -^ rootBox.leftTop

let setRightBottom vec2 root elems =
  let delta = vec2 -^ (getRightBottom root elems) in
  List.map (fun e -> Svg.move delta e) elems
  |> ignore

let getCenter root elems =
  let box = makeBox elems in
  let rootBox = Box.getRBox root in
  ((box.leftTop +^ box.rightBottom) /^ {x=2.0;y=2.0}) -^ rootBox.leftTop

let setCenter vec2 root elems =
  let delta = vec2 -^ (getCenter root elems) in
  List.map (fun e -> Svg.move delta e) elems
  |> ignore

let zoom ratio root elems =
  let center = getCenter root elems in
  elems
  |> List.map @@ fun e ->
    Svg.zoom ratio root e;
    let v = Svg.getCenter root e in
    Svg.setCenter
      ((v *^ ratio) +^ (({x=1.0;y=1.0} -^ ratio) *^ center))
      root e
  |> ignore

let getSize elems =
  let box = makeBox elems in
  box.rightBottom -^ box.leftTop

let setSize vec2 root elems =
  let ratio = vec2 /^ (getSize elems) in
  zoom ratio root elems

