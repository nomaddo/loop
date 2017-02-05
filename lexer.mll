(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Error of string

let loc = ref (-1, -1, -1)
let line = ref 1
let target = ref ""

let chars = ref 0
let line_chars = ref 1

let count lexbuf =
  let i = Lexing.lexeme_start lexbuf in
  let j = Lexing.lexeme_end lexbuf in
  j - i

let sum lexbuf =
  line_chars := !line_chars + count lexbuf

let update_loc lexbuf =
  let i = Lexing.lexeme_start lexbuf in
  let j = Lexing.lexeme_end lexbuf in
  loc := (!line, i - !chars + 1, j - !chars + 1)

let set_taget lexbuf =
  target := Lexing.lexeme lexbuf

let update lexbuf =
  sum lexbuf; update_loc lexbuf; set_taget lexbuf

let endline lexbuf =
  incr line;
  chars := !line_chars + !chars + 1;
  line_chars := 0

let mkhash l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun (s, k) -> Hashtbl.add h s k) l; h

let keyword_table =
  mkhash [
    "int",    INT;
    "real",   REAL;
    "void",   VOID;
    "if",     IF;
    "else",   ELSE;
    "for",    FOR;
    "to",     TO;
    "downto", DOWNTO;
    "by",     BY;
    "while",  WHILE;
  ]
}

let module_head = ['A'-'Z']
let ident_head = ['a'-'z']

let char = ['A'-'Z' 'a'-'z' '_' '0'-'9']

let ops  = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

let num = ['0' - '9']
let num_head = ['1' '2' '3' '4' '5' '6' '7' '8' '9']

rule token = parse
| [' ' '\t']        { incr line_chars; token lexbuf }
| ['\n']            { endline lexbuf; token lexbuf }
| (num_head num * '.' num * | '0' '.' num *) as str { update lexbuf; FLOAT str }
| (num_head num * | '0') as str { update lexbuf; INTEGER (int_of_string str) }
| '('               { update lexbuf; LPAREN }
| ')'               { update lexbuf; RPAREN }
| '['               { update lexbuf; LBRACKET }
| ']'               { update lexbuf; RBRACKET }
| '='               { update lexbuf; EQ }
| '{'               { update lexbuf; LBRACE }
| '}'               { update lexbuf; RBRACE }
| ';'               { update lexbuf; SEMI }
| ','               { update lexbuf; COMMA }
| '.'               { update lexbuf; DOT }
| ['=' '<' '>' '|' '&' '$'] ops * as str
  { update lexbuf; OP0 (Pident str) }
| ['@' '^'] ops * as str
  { update lexbuf; OP1 (Pident str) }
| ['+' '-'] ops * as str
  { update lexbuf; OP2 (Pident str) }
| ['*' '/' '%'] ops * as str
  { update lexbuf; OP3 (Pident str) }
| module_head char *       {
    let s = Lexing.lexeme lexbuf in
    update lexbuf;
    try
      Hashtbl.find keyword_table s
    with Not_found -> MODULE s
  }
| ident_head char *       {
    let s = Lexing.lexeme lexbuf in
    update lexbuf;
    try
      Hashtbl.find keyword_table s
    with Not_found -> IDENT s
  }
| eof               { EOF }
