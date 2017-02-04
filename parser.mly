%{
  open Pident
  open Ast
%}

%token EOF

%token COMMA SEMI LPAREN RPAREN
%token <string> IDENT
%token <string> MODULE
%token VOID INT REAL
%token LBRACE RBRACE
%token <int> INTEGER
%token EQ
%token LBRACKET RBRACKET
%token IF ELSE
%token FOR TO DOWNTO BY

%token WHILE
%token DOT

%token <Pident.path> OP0
%token <Pident.path> OP1
%token <Pident.path> OP2
%token <Pident.path> OP3

%left OP0
%left OP1
%left OP2
%left OP3

%type  <Ast.t> main
%start main

%type  <Pident.path> ident
%start ident

%%

main:
| list(top_decl) EOF { $1 }

top_decl:
| fundecl { $1 }
| global_var SEMI { $1 }

fundecl:
| typ IDENT args block
  { Fundef ($1, $2, $3, $4) }

global_var:
| typ; IDENT; option(EQ expr { $2 })
  { Global_var ($1, $2, $3) }

typ:
| VOID { Void }
| INT  { Int }
| REAL { Real }
| typ LBRACKET option(expr) RBRACKET { Array ($1, $3) }

args:
| LPAREN separated_list (COMMA, arg) RPAREN { $2 }

arg:
| typ IDENT { ($1, $2) }

decl:
| var_decl SEMI    { $1 }
| if_decl          { $1 }
| assign_decl SEMI { $1 }
| for_decl         { $1 }
| while_decl       { $1 }
| call_decl SEMI   { $1 }
| astore_decl SEMI { $1 }

var_decl:
| typ; IDENT; option(EQ expr { $2 })
  { Decl ($1, $2, $3) }

if_decl:
| IF LPAREN expr RPAREN block option(ELSE block { $2 })
  { If ($3, $5, $6) }

assign_decl:
| ident EQ expr { Assign ($1, $3) }

for_decl:
| FOR IDENT EQ expr direction expr option (BY expr { $2 }) block
  { For ($2, $4, $5, $6, $7, $8) }

while_decl:
| WHILE LPAREN expr RPAREN block { While ($3, $5) }

call_decl:
| ident LPAREN separated_list (COMMA, expr) RPAREN
  { Call ($1, $3)}

astore_decl:
| ident nonempty_list(LBRACKET expr RBRACKET { $2 }) EQ expr
  { Astore ($1, $2, $4) }

direction:
| TO { To }
| DOWNTO { Downto }

block:
| LBRACE list (decl) RBRACE { $2 }

expr:
| INTEGER       { Iconst $1 }
| expr OP0 expr { Call ($2, [$1; $3]) }
| expr OP1 expr { Call ($2, [$1; $3]) }
| expr OP2 expr { Call ($2, [$1; $3]) }
| expr OP3 expr { Call ($2, [$1; $3]) }
| ident         { Var $1 }
| ident LPAREN separated_list (COMMA, expr) RPAREN { Call ($1, $3) }
| ident nonempty_list(LBRACKET expr RBRACKET { $2 }) { Aref ($1, $2) }

ident:
| MODULE DOT ident  { Ppath ($1, $3)}
| IDENT             { Pident $1 }