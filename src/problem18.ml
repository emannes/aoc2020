open! Core
open Common

let solve subpart file_contents =
  let eval =
    match (subpart : Subpart.t) with
    | A -> Arithmetic.Expression.eval_equal_precedence
    | B -> Arithmetic.Expression.eval_addition_precedes_mult
  in
  List.sum
    (module Int)
    file_contents
    ~f:(fun line ->
      Arithmetic.parse_with_error (Lexing.from_string line) |> ok_exn |> eval)
  |> print_int
;;
