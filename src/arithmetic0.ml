open! Core

module Operation = struct
  type t =
    | Plus
    | Times
  [@@deriving sexp]

  let to_function = function
    | Plus -> ( + )
    | Times -> ( * )
  ;;
end

module Expression = struct
  type t =
    | Int of int
    | Expr of t * (Operation.t * t) list
  [@@deriving sexp]

  let rec eval_equal_precedence =
    let eval = eval_equal_precedence in
    function
    | Int i -> i
    | Expr (t0, []) -> eval t0
    | Expr (t0, (op, t1) :: rest) ->
      Expr (Int ((Operation.to_function op) (eval t0) (eval t1)), rest) |> eval
  ;;

  let rec eval_addition_precedes_mult =
    let eval = eval_addition_precedes_mult in
    function
    | Int i -> i
    | Expr (t0, []) -> eval t0
    | Expr (t0, [ (Times, t1) ]) -> eval t0 * eval t1
    | Expr (t0, (Plus, t1) :: rest) -> Expr (Int (eval t0 + eval t1), rest) |> eval
    | Expr (t0, (Times, t1) :: (Times, t2) :: rest) ->
      Expr (Int (eval t0 * eval t1), (Times, t2) :: rest) |> eval
    | Expr (t0, (Times, t1) :: (Plus, t2) :: rest) ->
      Expr (t0, (Times, Int (eval t1 + eval t2)) :: rest) |> eval
  ;;
end
