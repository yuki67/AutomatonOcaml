open MyExt
open ListExt
open RegexSyntax

type t =  RegexNFA.t

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let compile str =
  let rec compile_sub = function
    | Any -> RegexNFA.any
    | Char s -> RegexNFA.just s
    | Repeat re -> RegexNFA.repeat (compile_sub re)
    | Concat res -> RegexNFA.concat (map compile_sub res)
    | _ -> RegexNFA.any
  in compile_sub (parse str)

let run it str = RegexNFA.run it str
