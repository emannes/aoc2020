open! Core
open! Async

let print_int n = printf "%d\n" n
let parse_as_input s = String.split_lines s |> List.map ~f:String.strip
let positive_mod n k = ((n mod k) + k) mod k

let split_into_groups file_contents =
  List.map ~f:String.strip file_contents
  |> List.group ~break:(fun row1 row2 ->
         (not (String.is_empty row1)) && String.is_empty row2)
  |> List.map ~f:(List.filter ~f:(fun s -> not (String.is_empty s)))
  |> List.filter ~f:(fun l -> not (List.is_empty l))
;;

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
  let zero = 0, 0

  let frequency_map ts =
    List.map ts ~f:(fun t -> t, 1) |> Map.of_alist_reduce ~f:Int.( + )
  ;;
end
