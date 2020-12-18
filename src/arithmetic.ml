open Core
open Arithmetic_lexer
open Lexing
include Arithmetic0

let position_to_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  try Arithmetic_parser.expression Arithmetic_lexer.read lexbuf |> Or_error.return with
  | SyntaxError msg -> Or_error.error_string msg
  | Arithmetic_parser.Error ->
    Or_error.errorf "%s: syntax error" (position_to_string lexbuf)
;;

let%expect_test _ =
  List.iter [ "2"; "2 + 3"; "(4 + 5) * 2 + 1" ] ~f:(fun input ->
      Lexing.from_string input
      |> parse_with_error
      |> [%sexp_of: Expression.t Or_error.t]
      |> print_s);
  [%expect
    {|
    (Ok (Int 2))
    (Ok (Expr (Int 2) ((Plus (Int 3)))))
    (Ok (Expr (Expr (Int 4) ((Plus (Int 5)))) ((Times (Int 2)) (Plus (Int 1))))) |}]
;;
