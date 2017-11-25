open Webapi.Dom
open Mocha
open Svg

let () =
  let svgOpt = Document.getElementById "svg" DomRe.document in
  let svgroot = Option.get svgOpt in
  Element.insertAdjacentHTML AfterBegin
  "<circle id=\"c\" cx=\"20\" cy=\"20\" r=\"10\" style=\"fill: rgb(60, 120, 5); fill-opacity: 0.5\"></circle>" svgroot;
  let circle = 
    Document.getElementById "c" DomRe.document
    |> Option.get
    |> fun x -> {rootElem=svgroot; elem=x}
  in
  it "coordinates"
    (fun () ->
      let leftTop = getLeftTop circle in
      let center = getCenter circle in
      let rightBottom = getRightBottom circle in
      floatEq "leftTop" leftTop.x 10.0;
      floatEq "center" center.x 20.0;
      floatEq "rightBottom" rightBottom.x 30.0
    );
  it "color"
    (fun () ->
      let fillColor = getFillColor circle in
      match fillColor with
      | Color.Rgba rgba ->
        intEq "color r" rgba.r 60;
        intEq "color g" rgba.g 120;
        intEq "color b" rgba.b 5;
        floatEq "color a" rgba.a 0.5
      | _ -> fail "not Rgba"
    )