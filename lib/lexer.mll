{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let arg = (alpha|digit|'_'|'-'|'/'|'.'|'~'|'$')+
let whitespace = [' ' '\t']+
let pipe = '|'

rule read = parse
| whitespace { read lexbuf }
| arg { ARG(Lexing.lexeme lexbuf) }
| pipe { PIPE }
| eof { EOF }