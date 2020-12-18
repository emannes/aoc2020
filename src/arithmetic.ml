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

let for_tests =
  [ "2 * 3 + (4 * 5)"
  ; "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  ; "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
  ; "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  ]
  |> List.map ~f:(fun line -> Lexing.from_string line |> parse_with_error |> ok_exn)
;;

let%expect_test "parsing" =
  List.iter for_tests ~f:(fun expression -> print_s (Expression.sexp_of_t expression));
  [%expect
    {|
    (Expr (Int 2) ((Times (Int 3)) (Plus (Expr (Int 4) ((Times (Int 5)))))))
    (Expr (Int 5)
     ((Plus
       (Expr (Int 8)
        ((Times (Int 3)) (Plus (Int 9)) (Plus (Int 3)) (Times (Int 4))
         (Times (Int 3)))))))
    (Expr (Int 5)
     ((Times (Int 9))
      (Times
       (Expr (Int 7)
        ((Times (Int 3)) (Times (Int 3)) (Plus (Int 9)) (Times (Int 3))
         (Plus (Expr (Int 8) ((Plus (Int 6)) (Times (Int 4))))))))))
    (Expr
     (Expr (Expr (Int 2) ((Plus (Int 4)) (Times (Int 9))))
      ((Times (Expr (Int 6) ((Plus (Int 9)) (Times (Int 8)) (Plus (Int 6)))))
       (Plus (Int 6))))
     ((Plus (Int 2)) (Plus (Int 4)) (Times (Int 2)))) |}]
;;

let%expect_test "equal precedence" =
  List.iter for_tests ~f:(fun expression ->
      Expression.eval_equal_precedence expression |> printf "%d\n");
  [%expect {|
    26
    437
    12240
    13632 |}]
;;

let%expect_test "+ precedes *" =
  List.iter for_tests ~f:(fun expression ->
      Expression.eval_addition_precedes_mult expression |> printf "%d\n");
  [%expect {|
    46
    1445
    669060
    23340 |}]
;;
