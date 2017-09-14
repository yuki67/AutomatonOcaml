(* multibyte character *)
type mb_char = string
type t =
  | Any
  | Char of mb_char (* for multibyte characters *)
  | Repeat of t
  | Anyof of mb_char list
  | Concat of t list
