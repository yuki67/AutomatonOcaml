%{
  open RegexSyntax
  open MyExt
  open ListExt
  let literals lst =
    let rec literals_rec = function
      | Any -> ["."]
      | Char s -> [s]
      | Repeat re -> "*"::literals_rec re
      | Anyof lst -> "[" :: "]" :: lst
      | Concat lst -> "(" :: ")" :: unions (map literals_rec lst) in
    unions (map literals_rec lst)
%}

%token ASTER DOT
%token LPAR RPAR
%token LSPAR RSPAR
%token <string> ID
%token EOF

%start toplevel
%type <RegexSyntax.t> toplevel
%%

toplevel:
  | atom EOF    { $1 }
  | atoms EOF   { Concat ($1) }
  | EOF          { Concat ([]) }
;

atoms:
  | atom atoms           { $1::$2 }
  |                       { [] }
;

atom:
  | DOT                  { Any }
  | ID                   { Char ($1) }
  | atom ASTER           { Repeat ($1) }
  | LSPAR atoms RSPAR    { Anyof (literals $2) }
  | LPAR atoms RPAR      { Concat ($2) }
;
