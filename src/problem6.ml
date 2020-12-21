open! Core
open Async
open Common

let solve subpart file_contents =
  let groups = split_into_groups file_contents in
  match (subpart : Subpart.t) with
  | A ->
    List.sum
      (module Int)
      groups
      ~f:(fun qs -> String.concat qs |> String.to_list |> Char.Set.of_list |> Set.length)
    |> print_int
  | B ->
    List.sum
      (module Int)
      groups
      ~f:(fun qs ->
        let qs = List.filter qs ~f:(fun s -> not @@ String.is_empty s) in
        let num_people = List.length qs in
        List.sort (String.concat qs |> String.to_list) ~compare:Char.compare
        |> List.group ~break:Char.( <> )
        |> List.count ~f:(fun answers -> List.length answers = num_people))
    |> print_int
;;

let%expect_test _ =
  solve
    B
    (parse_as_input
       {|abc

      a
      b
      c
      
      ab
      ac
      
      a
      a
      a
      a
      
      b|});
  [%expect {|
    6 |}]
;;
