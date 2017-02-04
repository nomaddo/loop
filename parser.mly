%{
  open Ast
%}

%token EOF

%token COMMA SEMI LPAREN RPAREN
%token END
%token <string> IDENT
%token VOID INT REAL
%token LBRACE RBRACE
%token <int> INTEGER

%type  <Ast.t> main
%start main

%%

main:
| list(top_decl) EOF { $1 }

top_decl:
| fundecl { $1 }

fundecl:
| typ IDENT LPAREN separated_list(COMMA, var_decl) RPAREN
  separated_list(SEMI, decl) END
  { Fundef ($1, $2, $4, $6) }

typ:
| VOID { Void }
| INT  { Int }
| REAL { Real }
| typ LBRACE expr RBRACE { Array $1 }

var_decl:
| typ IDENT { ($1, $2) }

decl:
| typ IDENT { Decl ($1, $2) }

expr:
| INTEGER { Iconst $1 }