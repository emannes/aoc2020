open! Core
open Async
open Common

module Cube = struct
  module T = struct
    type t = int * int * int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let ( + ) (a1, a2, a3, a4) (b1, b2, b3, b4) = a1 + b1, a2 + b2, a3 + b3, a4 + b4

  let neighbor_offsets subpart : t list =
    let pm1 = [ -1; 0; 1 ] in
    List.cartesian_product pm1 pm1
    |> List.cartesian_product pm1
    |> List.cartesian_product
         (match (subpart : Subpart.t) with
         | A -> [ 0 ]
         | B -> pm1)
    |> List.filter_map ~f:(fun (d, (a, (b, c))) ->
           if List.exists ~f:Int.(( <> ) 0) [ a; b; c; d ]
           then Some (a, b, c, d)
           else None)
  ;;

  let neighbors subpart cube = List.map (neighbor_offsets subpart) ~f:(( + ) cube)
end

let step subpart (active_cubes : Cube.Set.t) =
  Set.to_list active_cubes
  |> List.concat_map ~f:(Cube.neighbors subpart)
  |> List.map ~f:(fun cube -> cube, 1)
  |> Cube.Map.of_alist_reduce ~f:( + )
  |> Map.filteri ~f:(fun ~key:cube ~data:active_neighbors ->
         if Set.mem active_cubes cube
         then active_neighbors = 2 || active_neighbors = 3
         else active_neighbors = 3)
  |> Map.key_set
;;

let parse file_contents =
  List.map file_contents ~f:String.to_list
  |> List.concat_mapi ~f:(fun x cubes ->
         List.filter_mapi cubes ~f:(fun y cube ->
             if Char.equal cube '#' then Some (x, y, 0, 0) else None))
  |> Cube.Set.of_list
;;

let solve subpart file_contents =
  let initial_state = parse file_contents in
  Fn.apply_n_times ~n:6 (step subpart) initial_state |> Set.length |> print_int
;;

let%expect_test _ =
  let file_contents = {|.#.
..#
###|} |> parse_as_input in
  solve A file_contents;
  let%bind () = [%expect {| 112 |}] in
  solve B file_contents;
  let%bind () = [%expect {| 848 |}] in
  return ()
;;
