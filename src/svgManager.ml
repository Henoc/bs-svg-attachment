type t = {
  rootElem: Natives.svgElement;
  elem: Natives.svgElement;
}

let getAttr t name =
  t.elem.getAttribute name
  |> Js.Nullable.to_opt

let setAttr t name value =
  t.elem.setAttribute name value

let deleteAttr t name =
  t.elem.setAttribute name [%bs.raw {| null |}]

let getBBox t =
  t.elem.getBoundingClientRect ()

let rec getLeftTop t =
  let box = getBBox t in
  let ground = getLeftTop {rootElem=t.rootElem; elem=t.elem} in
  Vec2.sub {x=box.left; y=box.top} ground
