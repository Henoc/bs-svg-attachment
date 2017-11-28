external describle: string -> (unit -> unit) -> unit = "" [@@bs.val]
external it: string -> (unit -> unit) -> unit = "" [@@bs.val]
external beforeEach: (unit -> unit) -> unit = "" [@@bs.val]

exception AssertionError of string

let shouldTrue label value =
  if value == false then raise (AssertionError ("at " ^ label))

let floatEq label value1 value2 =
  if value1 != value2 then
  let v1str = string_of_float value1 in
  let v2str = string_of_float value2 in
  raise (AssertionError ("at " ^ label ^ ", " ^ v1str ^ " not equal " ^ v2str))

let intEq label value1 value2 =
  if value1 != value2 then
  let v1str = string_of_int value1 in
  let v2str = string_of_int value2 in
  raise (AssertionError ("at " ^ label ^ ", " ^ v1str ^ " not equal " ^ v2str))

let stringEq label value1 value2 =
  if value1 != value2 then
  raise (AssertionError ("at " ^ label ^ ", " ^ value1 ^ " not equal " ^ value2))

let fail label = raise (AssertionError ("at " ^ label))
