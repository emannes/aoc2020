open! Core
open Common

type t =
  { bounds : int * int
  ; requirement : Char.t
  ; password : string
  }

let re =
  let open Re in
  seq
    [ group (rep digit)
    ; char '-'
    ; group (rep digit)
    ; char ' '
    ; group alpha
    ; str ": "
    ; group (rep notnl)
    ]
  |> whole_string
  |> compile
;;

let of_string s =
  let open Re in
  let groups = exec re s |> Group.all in
  { bounds = groups.(1) |> Int.of_string, groups.(2) |> Int.of_string
  ; requirement = groups.(3) |> Char.of_string
  ; password = groups.(4)
  }
;;

let is_satisfied_a { bounds = low, high; requirement; password } =
  let occurrences =
    String.to_list password |> List.filter ~f:(Char.equal requirement) |> List.length
  in
  Int.between ~low ~high occurrences
;;

let is_satisfied_b { bounds = pos1, pos2; requirement; password } =
  let safe_get n = if String.length password < n then None else Some password.[n - 1] in
  List.map ~f:safe_get [ pos1; pos2 ]
  |> List.filter_opt
  |> List.count ~f:(Char.equal requirement)
  |> ( = ) 1
;;

let solve subpart file_contents =
  let ts = List.map file_contents ~f:of_string in
  match (subpart : Subpart.t) with
  | A -> List.filter ts ~f:is_satisfied_a |> List.length |> print_int
  | B -> List.filter ts ~f:is_satisfied_b |> List.length |> print_int
;;
