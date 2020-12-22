open! Core
open Async
open Common

let winner deck1 deck2 =
  if Queue.length deck1 = 0
  then Some deck2
  else if Queue.length deck2 = 0
  then Some deck1
  else None
;;

let score deck =
  let deck = Queue.to_list deck in
  let length = List.length deck in
  List.mapi deck ~f:(fun i card -> (length - i) * card) |> List.sum (module Int) ~f:Fn.id
;;

module Regular_combat = struct
  let step deck1 deck2 : unit =
    let card1 = Queue.dequeue_exn deck1 in
    let card2 = Queue.dequeue_exn deck2 in
    if card1 > card2
    then Queue.enqueue_all deck1 [ card1; card2 ]
    else if card2 > card1
    then Queue.enqueue_all deck2 [ card2; card1 ]
    else failwith "no ties allowed"
  ;;

  let rec play deck1 deck2 =
    match winner deck1 deck2 with
    | Some deck -> deck
    | None ->
      step deck1 deck2;
      play deck1 deck2
  ;;
end

module Recursive_combat = struct
  let to_string deck1 deck2 =
    let deck_to_string deck =
      Queue.to_list deck |> List.map ~f:Int.to_string |> String.concat ~sep:","
    in
    deck_to_string deck1 ^ " |" ^ deck_to_string deck2
  ;;

  let rec play deck1 deck2 =
    let states_so_far = String.Hash_set.create () in
    let rec play' () =
      let state = to_string deck1 deck2 in
      if Hash_set.mem states_so_far state
      then `P1, deck1
      else if Queue.length deck1 = 0
      then `P2, deck2
      else if Queue.length deck2 = 0
      then `P1, deck1
      else (
        Hash_set.add states_so_far state;
        let card1 = Queue.dequeue_exn deck1 in
        let card2 = Queue.dequeue_exn deck2 in
        if card1 > Queue.length deck1 || card2 > Queue.length deck2
        then (
          if card1 > card2
          then Queue.enqueue_all deck1 [ card1; card2 ]
          else if card2 > card1
          then Queue.enqueue_all deck2 [ card2; card1 ]
          else failwith "no ties";
          play' ())
        else (
          let subdeck1 = List.take (Queue.to_list deck1) card1 in
          let subdeck2 = List.take (Queue.to_list deck2) card2 in
          (match play (Queue.of_list subdeck1) (Queue.of_list subdeck2) with
          | `P1, _ -> Queue.enqueue_all deck1 [ card1; card2 ]
          | `P2, _ -> Queue.enqueue_all deck2 [ card2; card1 ]);
          play' ()))
    in
    play' ()
  ;;
end

let solve subpart file_contents =
  let deck1, deck2 =
    let to_deck lines = List.tl_exn lines |> List.map ~f:Int.of_string |> Queue.of_list in
    match split_into_groups file_contents with
    | [ deck1; deck2 ] -> to_deck deck1, to_deck deck2
    | _ -> failwith "parse error"
  in
  match (subpart : Subpart.t) with
  | A ->
    let winning_deck = Regular_combat.play deck1 deck2 in
    score winning_deck |> print_int
  | B ->
    let winning_deck = Recursive_combat.play deck1 deck2 |> snd in
    score winning_deck |> print_int
;;

let%expect_test _ =
  let file_contents =
    {|Player 1:
  9
  2
  6
  3
  1
  
  Player 2:
  5
  8
  4
  7
  10|}
    |> parse_as_input
  in
  solve A file_contents;
  let%bind () = [%expect {| 306 |}] in
  solve B file_contents;
  let%bind () = [%expect {| 291 |}] in
  return ()
;;
