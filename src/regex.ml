open MyExt
open ListExt
open NFA
open RegexSyntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let compile str =
  let rec compile_sub = function
    | Any -> any
    | Char s -> just s
    | Repeat re -> repeat (compile_sub re)
    | _ -> any
  in compile_sub (parse str)
