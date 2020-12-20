open! Core
open Async
open Common

let parse file_contents =
  List.map file_contents ~f:(fun line ->
      match String.chop_suffix line ~suffix:" bags contain no other bags." with
      | Some bag -> bag, []
      | None ->
        (match String.split ~on:' ' line with
        | attr :: color :: "bags" :: "contain" :: rest ->
          ( attr ^ " " ^ color
          , List.chunks_of rest ~length:4
            |> List.map ~f:(fun l ->
                   ( Int.of_string (List.nth_exn l 0)
                   , List.nth_exn l 1 ^ " " ^ List.nth_exn l 2 )) )
        | _ -> failwith "parse error"))
;;

let can_have_parent rules =
  List.concat_map rules ~f:(fun (container, contained) ->
      List.map contained ~f:(fun (_n, contained) -> contained, container))
  |> String.Map.of_alist_multi
;;

let can_have_ancestor rules bag =
  let can_have_parent = can_have_parent rules in
  let rec can_have_ancestor' checked to_check =
    if Set.is_empty to_check
    then checked
    else (
      let parents =
        Set.to_list to_check
        |> List.concat_map ~f:(fun bag ->
               Map.find can_have_parent bag |> Option.value ~default:[])
        |> String.Set.of_list
      in
      can_have_ancestor' (Set.union checked to_check) parents)
  in
  can_have_ancestor' String.Set.empty (String.Set.singleton bag)
;;

(** includes self as a depth-0 descendant *)
let rec num_descendants rules bag =
  match Map.find rules bag with
  | None -> 1
  | Some children ->
    1
    + List.sum
        (module Int)
        children
        ~f:(fun (n, child) -> n * num_descendants rules child)
;;

let solve subpart file_contents =
  let rules = parse file_contents in
  match (subpart : Subpart.t) with
  | A -> Set.length (can_have_ancestor rules "shiny gold") - 1 |> print_int
  | B -> num_descendants (String.Map.of_alist_exn rules) "shiny gold" - 1 |> print_int
;;

let%expect_test "A" =
  let input =
    {|light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.|}
    |> parse_as_input
  in
  solve A input;
  [%expect {| 4 |}]
;;

let%expect_test "B" =
  let input =
    {|shiny gold bags contain 2 dark red bags.
    dark red bags contain 2 dark orange bags.
    dark orange bags contain 2 dark yellow bags.
    dark yellow bags contain 2 dark green bags.
    dark green bags contain 2 dark blue bags.
    dark blue bags contain 2 dark violet bags.
    dark violet bags contain no other bags.|}
    |> parse_as_input
  in
  solve B input;
  [%expect {| 126 |}]
;;
