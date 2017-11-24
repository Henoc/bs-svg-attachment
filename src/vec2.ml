type t = {
  x: float;
  y: float;
}

let (+^) a b =
  {x = a.x +. b.x; y = a.y +. b.y}

let (-^) a b =
  {x = a.x -. b.x; y = a.y -. b.y}

let ( *^ ) a b =
  {x = a.x *. b.x; y = a.y *. b.y}

let (/^) a b =
  {x = a.x /. b.x; y = a.y /. b.y}
