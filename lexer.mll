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

rule token = parse
| [' ' '\t']        { incr line_chars; token lexbuf }
| ['\n']            { endline lexbuf; token lexbuf }
| ['0'-'9']+ as lxm { update lexbuf; INTEGER (int_of_string lxm) }
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
