{
open Lexing
open Arithmetic_parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let int = '-'? digit+
let white = ' '+
let newline = '\r' | '\n' | "\r\n"

rule read = 
    parse
    | white {read lexbuf}
    | int {INT (int_of_string (Lexing.lexeme lexbuf))}
    | '+' { PLUS }
    | '*' { TIMES }
    | '(' { LEFT_PAREN }
    | ')' { RIGHT_PAREN }
    | eof { EOF }
