open! Core
open Async
open Common

let solve subpart file_contents =
  let adapters =
    let in_the_bag = List.map file_contents ~f:Int.of_string |> Int.Set.of_list in
    Set.add in_the_bag (3 + Set.max_elt_exn in_the_bag)
  in
  match (subpart : Subpart.t) with
  | A ->
    let joltage_sequence = Set.to_list (Set.add adapters 0) (* for the outlet *) in
    let jumps =
      List.zip_with_remainder joltage_sequence (List.tl_exn joltage_sequence)
      |> fst
      |> List.map ~f:(fun (smaller, larger) -> larger - smaller)
    in
    let ones, threes = List.count jumps ~f:(( = ) 1), List.count jumps ~f:(( = ) 3) in
    print_int (ones * threes)
  | B ->
    Set.fold adapters ~init:(Int.Map.singleton 0 1) ~f:(fun num_ways adapter ->
        Map.add_exn
          num_ways
          ~key:adapter
          ~data:
            (Map.filter_keys
               num_ways
               ~f:(Int.between ~low:(adapter - 3) ~high:(adapter - 1))
            |> Map.data
            |> List.reduce_exn ~f:( + )))
    |> Map.max_elt_exn
    |> snd
    |> print_int
;;

let%expect_test "small" =
  let input =
    {|16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4|}
    |> parse_as_input
  in
  solve A input;
  [%expect {| 35 |}]
;;

let%expect_test "large" =
  let input =
    {|28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3|}
    |> parse_as_input
  in
  solve A input;
  let%bind () = [%expect {| 220 |}] in
  solve B input;
  [%expect {| 19208 |}]
;;
