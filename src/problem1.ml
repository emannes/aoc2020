open! Core
open Common

let pairsumsto set target =
  Set.filter set ~f:(fun i -> Set.mem set (target - i)) |> Set.to_list |> List.hd
;;

let solve subpart file_contents =
  let expenses = List.map file_contents ~f:Int.of_string |> Int.Set.of_list in
  match (subpart : Subpart.t) with
  | A ->
    pairsumsto expenses 2020 |> Option.value_exn |> fun i -> print_int (i * (2020 - i))
  | B ->
    Set.iter expenses ~f:(fun i ->
        match pairsumsto expenses (2020 - i) with
        | Some expense -> print_int (i * expense * (2020 - i - expense))
        | None -> ())
;;
