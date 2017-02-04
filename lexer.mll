(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Error of string

let update _ = ()
let line_chars = ref 0
let endline _ = ()

let mkhash l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun (s, k) -> Hashtbl.add h s k) l; h

let keyword_table =
  mkhash [
    "int",  INT;
    "real", REAL;
    "void", VOID;
  ]


}

let head = ['A'-'Z' 'a'-'z']
let char = ['A'-'Z' 'a'-'z' '_' '0'-'9']

rule token = parse
| [' ' '\t' '\n']   { incr line_chars; token lexbuf } (* skip blanks *)
| ['0'-'9']+ as lxm { update lexbuf; INTEGER (int_of_string lxm) }
| '('               { update lexbuf; LPAREN }
| ')'               { update lexbuf; RPAREN }
| '['               { update lexbuf; LBRACE }
| ']'               { update lexbuf; RBRACE }
| head char *       {
    let s = Lexing.lexeme lexbuf in
    update lexbuf;
    try
      Hashtbl.find keyword_table s
    with Not_found -> IDENT s
  }
| eof               { EOF }
