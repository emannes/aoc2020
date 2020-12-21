open! Core
open Common

module Food = struct
  type t =
    { ingredients : String.Set.t
    ; allergens : String.Set.t
    }
  [@@deriving sexp, fields]

  let of_string s =
    let tokens = String.split ~on:' ' s in
    let ingredients =
      List.take_while tokens ~f:(String.( <> ) "(contains") |> String.Set.of_list
    in
    let allergens =
      List.drop tokens (Set.length ingredients + 1)
      |> List.map ~f:(String.strip ~drop:(String.contains "(),"))
      |> String.Set.of_list
    in
    { ingredients; allergens }
  ;;

  let%expect_test _ =
    let t = of_string "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)" in
    print_s [%message (t : t)];
    [%expect
      {| (t ((ingredients (kfcds mxmxvkd nhms sqjhc)) (allergens (dairy fish)))) |}]
  ;;
end

let solve subpart file_contents =
  let foods = List.map ~f:Food.of_string file_contents in
  let all_allergens = List.map foods ~f:Food.allergens |> String.Set.union_list in
  let potential_ingredients =
    Set.to_map all_allergens ~f:(fun allergen ->
        List.filter foods ~f:(fun food -> Set.mem food.allergens allergen)
        |> List.map ~f:Food.ingredients
        |> List.reduce_exn ~f:Set.inter)
  in
  let all_potential_ingredients =
    Map.data potential_ingredients |> String.Set.union_list
  in
  match (subpart : Subpart.t) with
  | A ->
    List.sum
      (module Int)
      foods
      ~f:(fun food ->
        Set.count food.ingredients ~f:(fun ingredient ->
            not (Set.mem all_potential_ingredients ingredient)))
    |> print_int
    (* I did the rest by hand *)
  | B -> print_s [%message (potential_ingredients : String.Set.t String.Map.t)]
;;

(* 
dairy,cfzdnz
eggs,htxsjf
fish,ttbrlvd
nuts,bbbl
peanuts,lmds
shellfish,cbmjz
soy,cmbcm
wheat,dvnbh

cfzdnz,htxsjf,ttbrlvd,bbbl,lmds,cbmjz,cmbcm,dvnbh

*)
