open Webapi.Dom
open Mocha
open SvgManager

let () =
  let svgOpt = Document.getElementById "svg" DomRe.document in
  let svgroot = Option.get svgOpt in
  Element.insertAdjacentHTML AfterBegin
  "<circle id=\"c\" cx=\"20\" cy=\"20\" r=\"10\"></circle>" svgroot;
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
      assertion (leftTop.x == 10.0);
      assertion (center.x == 20.0);
      assertion (rightBottom.x == 30.0)
    )