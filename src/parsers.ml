open! BsLittleParser.Parser
open BsLittleParser

let next regex s =
  let resultOpt = Js.Re.exec s regex in
    match resultOpt with
    | None -> None
    | Some result ->
      Js.Nullable.to_opt (Js.Re.captures result).(0)

let getAllFloats s =
  let floatRegex = [%re "/[+-]?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?/g"] in
  let rec loop s acc =
    match next floatRegex s with
    | None -> acc
    | Some matched ->
      loop s (matched :: acc)
  in
  loop s [] |> List.rev |> List.map float_of_string

let getAllInts s =
  let intRegex = [%re "/[+-]?\\d+/g"] in
  let rec loop s acc =
    match next intRegex s with
    | None -> acc
    | Some matched ->
      loop s (matched :: acc)
  in
  loop s [] |> List.rev |> List.map int_of_string

let parseRgb s =
  let ints = getAllInts s |> Array.of_list in
  Color.Rgb {r=ints.(0); g=ints.(1); b=ints.(2)}

let parseRgba s =
  let floats = getAllFloats s |> Array.of_list in
  Color.Rgba {
    r = int_of_float floats.(0);
    g = int_of_float floats.(1);
    b = int_of_float floats.(2);
    a = floats.(3)
  }

let genColor (color: Color.t) = match color with
  | Color.None -> "none"
  | Color.Rgb rgb ->
    "rgb(" ^
    (string_of_int rgb.r) ^ "," ^
    (string_of_int rgb.g) ^ "," ^
    (string_of_int rgb.b) ^ ")"
  | Color.Rgba rgba ->
    "rgba(" ^
    (string_of_int rgba.r) ^ "," ^
    (string_of_int rgba.g) ^ "," ^
    (string_of_int rgba.b) ^ "," ^
    (string_of_float rgba.a) ^ ")"

let parsePoints s =
  let floats = getAllFloats s in
  let rec loop floats acc = match floats with
  | x :: y :: tail -> loop tail (Vec2.{x = x; y = y} :: acc)
  | _ -> acc
  in
  loop floats []
  |> List.rev

let genPoints points =
  let toStr (vec2: Vec2.t) =
    (string_of_float vec2.x) ^ " " ^ (string_of_float vec2.y)
  in
  let rec loop (points: Vec2.t list) acc = match points with
  | hd :: tl -> loop tl (acc ^ ", " ^ (toStr hd))
  | [] -> acc
  in
  loop (List.tl points) (toStr @@ List.hd points)

let parseD s =
  let opParser = regex [%re "/[mMlLhHvVaAqQtTcCsSzZ]/"] in
  let floatParser = regex [%re "/[+-]?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?/"] in
  let floatPairParser = (floatParser <*> floatParser) ^^ (fun (x, y) -> Vec2.{x = float_of_string x; y = float_of_string y}) in
  let pathParser = rep ((opParser <*> (rep floatPairParser)) ^^ (fun (x, y) -> Path.{operator = x; points = y}) ) in
  match pathParser Input.{text = s; index = 0; whitespace = " ,\n"} with
  | ParseResult.ParseFailure _ -> Js.log "fail"; []
  | ParseResult.ParseSuccess (r, _) -> Js.log "success"; r

let genD operators =
  let toStr (op: Path.t) =
    op.operator ^ " " ^ (genPoints op.points)
  in
  let rec loop (ops: Path.t list) acc = match ops with
  | hd :: tl -> loop tl (acc ^ ", " ^ (toStr hd))
  | [] -> acc
  in
  loop (List.tl operators) (toStr @@ List.hd operators)
