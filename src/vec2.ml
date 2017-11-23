type t = {
  x: float;
  y: float;
}

let add a b =
  {x = a.x +. b.x; y = a.y +. b.y}

let sub a b =
  {x = a.x -. b.x; y = a.y -. b.y}


