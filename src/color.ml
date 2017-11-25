
type rgba = {
  r: int;
  g: int;
  b: int;
  a: float;
}

type rgb = {
  r: int;
  g: int;
  b: int;
}

type t = None | Rgba of rgba | Rgb of rgb