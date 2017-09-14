%{
  open RegexSyntax
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
  | LSPAR atoms RSPAR    { Anyof (Regex.literals $2) }
  | LPAR atoms RPAR      { Concat ($2) }
;
