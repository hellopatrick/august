type t =
  | Exec of string list
  | Pipe of
      { from : t
      ; into : t
      }
[@@deriving show]
