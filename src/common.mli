open! Core

val print_int : int -> unit
val parse_as_input : string -> string list
val positive_mod : int -> int -> int
val split_into_groups : string list -> string list list

module Subpart : sig
  type t =
    | A
    | B
  [@@deriving sexp]
end

module Pair : sig
  type t = int * int [@@deriving sexp, compare]

  include Comparable.S with type t := t
  include Container.Summable with type t := t
end
