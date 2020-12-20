open! Core
open Common

let count file_contents (right, down) =
  List.chunks_of ~length:down file_contents
  |> List.map ~f:List.hd_exn
  |> List.counti ~f:(fun i row -> Char.equal '#' row.[right * i mod String.length row])
;;

let solve subpart file_contents =
  match (subpart : Subpart.t) with
  | A -> count file_contents (3, 1) |> print_int
  | B ->
    List.map [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ] ~f:(count file_contents)
    |> List.reduce_exn ~f:( * )
    |> print_int
;;
