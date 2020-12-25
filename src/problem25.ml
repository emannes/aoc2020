open! Core
open! Common

let base = 7
let modulus = 20201227

let discrete_log n =
  let rec discrete_log' running_product exponent =
    if running_product = n
    then exponent
    else discrete_log' (base * running_product mod modulus) (exponent + 1)
  in
  discrete_log' 7 1
;;

let discrete_exp b n =
  let rec discrete_exp' running_product exponent =
    if exponent = n
    then running_product
    else discrete_exp' (b * running_product mod modulus) (exponent + 1)
  in
  discrete_exp' 1 0
;;

let%expect_test _ =
  print_int (discrete_log 5764801);
  print_int (discrete_log 17807724);
  [%expect {|
    8
    11 |}]
;;

let solve subpart file_contents =
  let door_pub, card_pub =
    match List.map ~f:Int.of_string file_contents with
    | [ door_pub; card_pub ] -> door_pub, card_pub
    | _ -> failwith "parse error"
  in
  let door_priv = discrete_log door_pub in
  let card_priv = discrete_log card_pub in
  print_s [%message (door_priv : int) (card_priv : int)];
  match (subpart : Subpart.t) with
  | A -> discrete_exp 7 (door_priv * card_priv mod (modulus - 1)) |> print_int
  | B -> failwith "not implemented"
;;

let%expect_test _ =
  let file_contents = [ "5764801"; "17807724" ] in
  solve A file_contents;
  [%expect {|
    ((door_priv 8) (card_priv 11))
    14897079 |}]
;;
