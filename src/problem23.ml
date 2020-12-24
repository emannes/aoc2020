open! Core
open Common

module State = struct
  type t =
    { cup_by_position : int Float.Map.t
    ; position_by_cup : float Int.Map.t
    ; current_cup : int
    }
  [@@deriving sexp]

  let of_list cups =
    let num_cups = List.length cups in
    let cups_n_positions =
      List.mapi cups ~f:(fun i cup -> Float.of_int (i + num_cups), cup)
    in
    { cup_by_position = Float.Map.of_alist_exn cups_n_positions
    ; position_by_cup =
        Int.Map.of_alist_exn (List.map cups_n_positions ~f:(fun (a, b) -> b, a))
    ; current_cup = List.hd_exn cups
    }
  ;;

  let closest_key_wrapping map how key =
    match Map.closest_key map how key with
    | Some (next_key, value) -> next_key, value
    | None ->
      (match how with
      | `Greater_or_equal_to | `Greater_than -> Map.min_elt_exn map
      | `Less_or_equal_to | `Less_than -> Map.max_elt_exn map)
  ;;

  let remove_all map keys = List.fold keys ~init:map ~f:Map.remove

  let step { cup_by_position; position_by_cup; current_cup } =
    let current_cup_position = Map.find_exn position_by_cup current_cup in
    let next_cup_after position =
      closest_key_wrapping cup_by_position `Greater_than position
    in
    let next_position, next_cup = next_cup_after current_cup_position in
    let next'_position, next'_cup = next_cup_after next_position in
    let next''_position, next''_cup = next_cup_after next'_position in
    let cup_by_position =
      remove_all cup_by_position [ next_position; next'_position; next''_position ]
    in
    let position_by_cup =
      remove_all position_by_cup [ next_cup; next'_cup; next''_cup ]
    in
    let _destination_cup, destination_position =
      closest_key_wrapping position_by_cup `Less_than current_cup
    in
    let post_destination_position, _post_destination_cup =
      closest_key_wrapping cup_by_position `Greater_than destination_position
    in
    let new_position, new'_position, new''_position =
      let post_destination_position =
        if Float.( < ) destination_position post_destination_position
        then post_destination_position
        else destination_position +. 1.
      in
      let step = (post_destination_position -. destination_position) /. 4. in
      ( destination_position +. step
      , destination_position +. (2. *. step)
      , destination_position +. (3. *. step) )
    in
    let cup_by_position =
      Map.add_exn cup_by_position ~key:new_position ~data:next_cup
      |> Map.add_exn ~key:new'_position ~data:next'_cup
      |> Map.add_exn ~key:new''_position ~data:next''_cup
    in
    let position_by_cup =
      Map.add_exn position_by_cup ~data:new_position ~key:next_cup
      |> Map.add_exn ~data:new'_position ~key:next'_cup
      |> Map.add_exn ~data:new''_position ~key:next''_cup
    in
    let _new_current_cup_position, new_current_cup =
      closest_key_wrapping cup_by_position `Greater_than current_cup_position
    in
    print_s
      [%message
        (current_cup : int)
          (current_cup_position : float)
          (next_cup : int)
          (next_position : float)
          (next'_cup : int)
          (next'_position : float)
          (next''_cup : int)
          (next''_position : float)
          (destination_position : float)
          (post_destination_position : float)
          (new_current_cup : int)];
    { cup_by_position; position_by_cup; current_cup = new_current_cup }
  ;;

  let starting_from_1 t =
    let index_of_1 =
      List.findi t ~f:(fun _index cup -> cup = 1) |> Option.value_exn |> fst
    in
    let before_1 = List.take t index_of_1 in
    let after_1 = List.drop t (index_of_1 + 1) in
    after_1 @ before_1
  ;;

  let to_A_answer t =
    Map.data t.cup_by_position
    |> starting_from_1
    |> List.map ~f:Int.to_string
    |> String.concat
  ;;

  let to_B_answer t =
    Map.data t.cup_by_position
    |> starting_from_1
    |> (fun cups -> List.take cups 2)
    |> List.reduce_exn ~f:( * )
  ;;

  (*   let step : t -> t = function
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
 *)
  let%expect_test _ =
    let test_input = [ 3; 8; 9; 1; 2; 5; 4; 6; 7 ] |> of_list in
    print_s [%message (step test_input : t)];
    print_s [%message (to_A_answer (Fn.apply_n_times ~n:10 step test_input) : string)];
    print_s [%message (to_A_answer (Fn.apply_n_times ~n:100 step test_input) : string)];
    [%expect
      {|
      ("step test_input"
       ((cup_by_position
         ((9 3) (13 2) (13.25 8) (13.5 9) (13.75 1) (14 5) (15 4) (16 6) (17 7)))
        (position_by_cup
         ((1 13.75) (2 13) (3 9) (4 15) (5 14) (6 16) (7 17) (8 13.25) (9 13.5)))
        (current_cup 2)))
      ("to_A_answer (Fn.apply_n_times ~n:10 step test_input)" 92658374)
      ("to_A_answer (Fn.apply_n_times ~n:100 step test_input)" 67384529) |}]
  ;;
end

let solve subpart _file_contents =
  let input = [ 9; 6; 3; 2; 7; 5; 4; 8; 1 ] in
  match (subpart : Subpart.t) with
  | A ->
    Fn.apply_n_times ~n:100 State.step (State.of_list input)
    |> State.to_A_answer
    |> print_endline
  | B ->
    (*     let num_cups, num_steps = 1_000_000, 23 in
 *)
    let num_cups, num_steps = 1_000, 28 in
    let input =
      input
      @ List.init (num_cups - List.length input) ~f:(fun i -> 1 + i + List.length input)
    in
    Fn.apply_n_times ~n:num_steps State.step (State.of_list input)
    |> State.to_B_answer
    |> print_int
;;
