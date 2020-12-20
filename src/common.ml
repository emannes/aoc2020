open! Core
open! Async

let print_int n = printf "%d\n" n
let parse_as_input s = String.split_lines s |> List.map ~f:String.strip
let positive_mod n k = ((n mod k) + k) mod k

module Subpart = struct
  type t =
    | A
    | B
  [@@deriving sexp]
end

module Pair = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
end
