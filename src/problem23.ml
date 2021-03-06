open! Core
open Common
module HLL = Hash_linked_list

module State = struct
  (* front = current cup, reading clockwise *)
  type t = int list

  let starting_from_1 t =
    let index_of_1 =
      List.findi t ~f:(fun _index cup -> cup = 1) |> Option.value_exn |> fst
    in
    let before_1 = List.take t index_of_1 in
    let after_1 = List.drop t (index_of_1 + 1) in
    after_1 @ before_1
  ;;

  let to_A_answer t = starting_from_1 t |> List.map ~f:Int.to_string |> String.concat

  let to_B_answer t =
    starting_from_1 t |> (fun cups -> List.take cups 2) |> List.reduce_exn ~f:( * )
  ;;

  let step : t -> t = function
    | current_cup :: next :: next' :: next'' :: rest ->
      let next_three = [ next; next'; next'' ] in
      let destination_cup =
        match
          List.filter rest ~f:(fun i -> i < current_cup)
          |> List.max_elt ~compare:Int.compare
        with
        | Some destination_cup -> destination_cup
        | None -> List.max_elt rest ~compare:Int.compare |> Option.value_exn
      in
      let endswith_dest, after_dest =
        let dest_index =
          List.findi rest ~f:(fun _index cup -> cup = destination_cup)
          |> Option.value_exn
          |> fst
        in
        List.split_n rest (dest_index + 1)
      in
      (* print_s
        [%message
          (endswith_dest : int list)
            (next_three : int list)
            (after_dest : int list)
            (destination_cup : int)
            (current_cup : int)]; *)
      endswith_dest @ next_three @ after_dest @ [ current_cup ]
    | _ -> failwith "huh? not enough cups"
  ;;

  let%expect_test _ =
    let test_input = [ 3; 8; 9; 1; 2; 5; 4; 6; 7 ] in
    print_s [%message (step test_input : int list)];
    print_s [%message (to_A_answer (Fn.apply_n_times ~n:10 step test_input) : string)];
    print_s [%message (to_A_answer (Fn.apply_n_times ~n:100 step test_input) : string)];
    print_s [%message (to_B_answer (Fn.apply_n_times ~n:10 step test_input) : int)];
    [%expect
      {|
      ("step test_input" (2 8 9 1 5 4 6 7 3))
      ("to_A_answer (Fn.apply_n_times ~n:10 step test_input)" 92658374)
      ("to_A_answer (Fn.apply_n_times ~n:100 step test_input)" 67384529) |}]
  ;;
end

let step (hll : HLL.t) : HLL.t =
  let next = HLL.after hll hll.current in
  let next' = HLL.after hll next in
  let next'' = HLL.after hll next' in
  List.iter [ next; next'; next'' ] ~f:(HLL.remove_exn hll);
  let destination, _ =
    if Map.min_elt_exn hll.element_map |> fst = hll.current
    then Map.max_elt_exn hll.element_map
    else Map.closest_key hll.element_map `Less_than hll.current |> Option.value_exn
  in
  HLL.insert_exn hll next ~just_after:destination;
  HLL.insert_exn hll next' ~just_after:next;
  HLL.insert_exn hll next'' ~just_after:next';
  { hll with current = HLL.after hll hll.current }
;;

let solve subpart _file_contents =
  let input = [ 9; 6; 3; 2; 7; 5; 4; 8; 1 ] in
  match (subpart : Subpart.t) with
  | A -> Fn.apply_n_times ~n:100 State.step input |> State.to_A_answer |> print_endline
  | B ->
    let input =
      input
      @ List.init (1_000_000 - List.length input) ~f:(fun i -> 1 + i + List.length input)
    in
    let end_state = Fn.apply_n_times ~n:10_000_000 step (HLL.of_list input) in
    let next = HLL.after end_state 1 in
    let next' = HLL.after end_state next in
    print_int (next * next')
;;
