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
  it "coordinates" @@
    fun () ->
      floatEq "leftTop" (getLeftTop @@ circle ()).x 10.0;
      floatEq "center" (getCenter @@ circle ()).x 20.0;
      floatEq "rightBottom" (getRightBottom @@ circle ()).x 30.0;
      setLeftTop Vec2.{x = 0.0; y = 10.0} @@ circle ();
      floatEq "leftTop'" (getLeftTop @@ circle ()).x 0.0;
      floatEq "center'" (getCenter @@ circle ()).x 10.0;
      floatEq "rightBottom'" (getRightBottom @@ circle ()).x 20.0;
    ;
  it "color" @@
    fun () ->
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
    ;
  it "parsers" @@
    fun () ->
      (match Parsers.parseRgb "rgb(5, 10, 15)" with
      | Color.Rgb rgb ->
        intEq "color r" rgb.r 5;
        intEq "color g" rgb.g 10;
        intEq "color b" rgb.b 15;
      | _ -> fail "not Rgb"
      );
      (match Parsers.parseRgba "rgba(5, 10, 15, 0.1)" with
      | Color.Rgba rgba ->
        intEq "color r" rgba.r 5;
        intEq "color g" rgba.g 10;
        intEq "color b" rgba.b 15;
        floatEq "color a" rgba.a 0.1;
      | _ -> fail "not Rgba"
      );
      (match Parsers.parsePoints "5 6, 7 8, 9 10" with
      | one :: two :: three :: [] ->
        floatEq "point 0 x" one.x 5.;
        floatEq "point 0 y" one.y 6.;
        floatEq "point 1 x" two.x 7.;
        floatEq "point 1 y" two.y 8.;
        floatEq "point 2 x" three.x 9.;
        floatEq "point 2 y" three.y 10.;
      | _ -> fail "not 3 points"
      );
      (match Parsers.parseD "M10 10 C 20 20, 40 20, 50 10" with
      | one :: two :: [] ->
        stringEq "path 0 op" one.operator "M";
        stringEq "path 1 op" two.operator "C"
      | _ -> fail "not path")