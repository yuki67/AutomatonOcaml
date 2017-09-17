%{
  open RegexSyntax
%}

%token BAR
%token LPAR RPAR
%token LSPAR RSPAR
%token ASTER DOT
%token <string> ID
%token EOF

%start toplevel
%type <RegexSyntax.t> toplevel
%%

toplevel:
  | regex EOF              { $1 }
;

regex:
  | alternative            { $1 }
;

alternative:
  | concat BAR alternative { cc_anyof $1 $3 }
  | concat                 { $1 }
;

concat:
  | atom concat            { cc_concat $1 $2 }
  | atom                   { $1 }
;


atom:
  | DOT                    { Any }
  | ID                     { Char ($1) }
  | atom ASTER             { Repeat ($1) }
  | LSPAR regex RSPAR      { Anyof (literals $2) }
  | LPAR regex RPAR        { $2 }
;