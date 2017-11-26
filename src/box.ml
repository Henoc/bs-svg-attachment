
(** Simple client rect box type *)
type t = {
  leftTop: Vec2.t;
  rightBottom: Vec2.t;
}

let getRBox elem =
  let rect = Natives.getBoundingClientRect elem in
  {
    leftTop = Vec2.{x = rect##left; y = rect##top};
    rightBottom = Vec2.{x = rect##right; y = rect##bottom};
  }

let merge a b =
  let left = min a.leftTop.x b.leftTop.x in
  let top = min a.leftTop.y b.leftTop.y in
  let right = max a.rightBottom.x b.rightBottom.x in
  let bottom = max a.rightBottom.y b.rightBottom.y in
  {
    leftTop = Vec2.{x = left; y = top};
    rightBottom = Vec2.{x = right; y = bottom};
  }