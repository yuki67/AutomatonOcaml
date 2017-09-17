open MyExt
open ListExt

(* multibyte character *)
type mb_char = string
type t =
  | Any
  | Char of mb_char (* for multibyte characters *)
  | Repeat of t
  | Anyof of t list
  | Concat of t list

(* "cc" for "concat" *)
let cc_concat x = function
  | Concat lst -> Concat (x::lst)
  | y -> Concat ([x; y])

let cc_anyof x = function
  | Anyof lst -> Anyof (x::lst)
  | y -> Anyof ([x; y])

let literals re =
  let rec literals_rec = function
    | Any -> [Char "."]
    | Char s as c -> [c]
    | Repeat re -> (Char "*")::literals_rec re
    | Anyof lst -> (Char "[") :: (Char "]") :: lst
    | Concat lst -> (Char  "(") :: (Char ")") :: unions (map literals_rec lst) in
  literals_rec re
