open! Core
open Async
open Common

let nth_turn starting_numbers num_turns =
  let last_said_on =
    List.mapi starting_numbers ~f:(fun turn number -> number, turn + 1)
    |> Int.Map.of_alist_exn
  in
  let rec eval turns_so_far last_said last_said_on =
    if turns_so_far = num_turns
    then last_said
    else (
      let number_to_say =
        match Map.find last_said_on last_said with
        | None -> 0
        | Some turn -> turns_so_far - turn
      in
      eval
        (turns_so_far + 1)
        number_to_say
        (Map.set last_said_on ~key:last_said ~data:turns_so_far))
  in
  let last_said = List.last_exn starting_numbers in
  eval (List.length starting_numbers) last_said (Map.remove last_said_on last_said)
;;

let%expect_test _ =
  List.iter [ 3; 4; 5; 6; 7; 8; 9; 10 ] ~f:(fun n -> nth_turn [ 0; 3; 6 ] n |> print_int);
  [%expect {|
    6
    0
    3
    3
    1
    0
    4
    0 |}]
;;

let solve subpart _file_contents =
  let input = [ 0; 1; 4; 13; 15; 12; 16 ] in
  match (subpart : Subpart.t) with
  | A -> print_int (nth_turn input 2020)
  | B -> print_int (nth_turn input 30000000)
;;
