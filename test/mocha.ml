external describle: string -> (unit -> unit) -> unit = "" [@@bs.val]
external it: string -> (unit -> unit) -> unit = "" [@@bs.val]

type assertion = bool -> unit
external assertion: assertion = "yeoman-assert" [@@bs.module]
