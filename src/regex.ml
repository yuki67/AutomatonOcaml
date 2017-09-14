open MyExt
open ListExt
open NFA
open RegexSyntax

let literals lst =
  let rec literals_rec = function
    | Any -> ["."]
    | Char s -> [s]
    | Repeat re -> "*"::literals_rec re
    | Anyof lst -> "[" :: "]" :: lst
    | Concat lst -> "(" :: ")" :: unions (map literals_rec lst) in
  unions (map literals_rec lst)

let rec compile = function
  | Any -> any
  | Char s -> just s
  | _ -> any
