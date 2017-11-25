open Webapi.Dom
open Mocha
open Svg

let () =
  let svgOpt = Document.getElementById "svg" DomRe.document in
  let svgroot = Option.get svgOpt in
  Element.insertAdjacentHTML AfterBegin
    "<circle id=\"c\"></circle>" svgroot;
  let circle () = 
    Document.getElementById "c" DomRe.document
    |> Option.get
  in
  beforeEach (fun () ->
    let c = Document.getElementById "c" DomRe.document |> Option.get in
    Element.setAttribute "cx" "20" c;
    Element.setAttribute "cy" "20" c;
    Element.setAttribute "r" "10" c;
    Element.setAttribute "style" "fill: rgb(60, 120, 5); fill-opacity: 0.5" c
  );
  it "coordinates"
    (fun () ->
      floatEq "leftTop" (getLeftTop svgroot @@ circle ()).x 10.0;
      floatEq "center" (getCenter svgroot @@ circle ()).x 20.0;
      floatEq "rightBottom" (getRightBottom svgroot @@ circle ()).x 30.0;
      setLeftTop Vec2.{x = 0.0; y = 10.0} svgroot @@ circle ();
      floatEq "leftTop'" (getLeftTop svgroot @@ circle ()).x 0.0;
      floatEq "center'" (getCenter svgroot @@ circle ()).x 10.0;
      floatEq "rightBottom'" (getRightBottom svgroot @@ circle ()).x 20.0;
    );
  it "color"
    (fun () ->
      (match getFillColor @@ circle () with
      | Color.Rgba rgba ->
        intEq "color r" rgba.r 60;
        intEq "color g" rgba.g 120;
        intEq "color b" rgba.b 5;
        floatEq "color a" rgba.a 0.5
      | _ -> fail "not Rgba");
      setFillColor (Color.Rgba Color.{r = 10; g = 20; b = 30; a = 0.6}) @@ circle ();
      (match getFillColor @@ circle () with
      | Color.Rgba rgba ->
        intEq "color r" rgba.r 10;
        intEq "color g" rgba.g 20;
        intEq "color b" rgba.b 30;
        floatEq "color a" rgba.a 0.6
      | _ -> fail "not Rgba");
    )