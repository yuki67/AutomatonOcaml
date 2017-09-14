let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = (alpha | digit)

rule main = parse
| space+       { main lexbuf }
| "*"          { Parser.ASTER }
| "."          { Parser.DOT }
| "("          { Parser.LPAR }
| ")"          { Parser.RPAR }
| "["          { Parser.LSPAR }
| "]"          { Parser.RSPAR }
| ident as id  { Parser.ID (MyExt.StringExt.of_char id) }
| eof          { Parser.EOF }
| _            { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
