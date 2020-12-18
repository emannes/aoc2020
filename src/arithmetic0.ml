open! Core

module Operation = struct
  type t =
    | Plus
    | Times
  [@@deriving sexp]
end

module Expression = struct
  type t =
    | Int of int
    | Expr of t * (Operation.t * t) list
  [@@deriving sexp]
end
