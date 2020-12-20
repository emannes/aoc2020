open Core
open Async
open Common

(* The data in [recents] is the # of occurrences in the last [preamble_length] *)
type t =
  { preamble_length : int
  ; trailing : int array
  ; idx_of_oldest : int ref
  ; frequencies : int Int.Table.t
  ; num_ints_processed : int ref
  }
[@@deriving sexp]

open Continue_or_stop

let nth_oldest t n =
  if Int.between n ~low:0 ~high:t.preamble_length
  then t.trailing.((!(t.idx_of_oldest) + n) mod t.preamble_length)
  else failwith "out of bounds"
;;

let init ~preamble_length message =
  let preamble, rest = List.split_n message preamble_length in
  let frequencies =
    List.map preamble ~f:(fun i -> i, 1)
    |> Int.Map.of_alist_reduce ~f:( + )
    |> Map.to_alist
    |> Int.Table.of_alist_exn
  in
  ( { preamble_length
    ; trailing = Array.of_list preamble
    ; idx_of_oldest = ref 0
    ; frequencies
    ; num_ints_processed = ref preamble_length
    }
  , rest )
;;

let step
    ({ preamble_length; trailing; idx_of_oldest; frequencies; num_ints_processed } as t)
    next_value
  =
  match
    Hashtbl.existsi frequencies ~f:(fun ~key ~data:_ ->
        2 * key <> next_value && Hashtbl.mem frequencies (next_value - key))
  with
  | false -> Stop next_value
  | true ->
    let oldest_value = nth_oldest t 0 in
    trailing.(!idx_of_oldest) <- next_value;
    idx_of_oldest := (!idx_of_oldest + 1) mod preamble_length;
    num_ints_processed := !num_ints_processed + 1;
    Hashtbl.change frequencies oldest_value ~f:(function
        | None -> failwith "Tried to remove a value that doesn't exist"
        | Some frequency -> if frequency > 1 then Some (frequency - 1) else None);
    Hashtbl.update frequencies next_value ~f:(function
        | None -> 1
        | Some frequency -> frequency + 1);
    Continue ()
;;

let solve' ~preamble_length subpart file_contents =
  let message = List.map ~f:Int.of_string file_contents in
  let t, rest = init ~preamble_length message in
  let invalid_number =
    List.fold_until
      rest
      ~init:()
      ~f:(fun _ next_value -> step t next_value)
      ~finish:(fun () -> failwith "no invalid number found")
  in
  match (subpart : Subpart.t) with
  | A -> print_int invalid_number
  | B ->
    (* for faster indexing *)
    let message = Array.of_list message in
    let num_ints_processed = !(t.num_ints_processed) in
    List.filter_map
      (* [invalid_number] is not processed *)
      (List.init (num_ints_processed - 1) ~f:Fn.id)
      ~f:(fun start_at_nth_num ->
        (* See if there's a contiguous set starting at the nth_oldest value *)
        let start_value = message.(start_at_nth_num) in
        List.fold_until
          (List.init
             (* if you start with the 2nd number before [invalid_value], you only have 1 upper bound to check *)
             (num_ints_processed - start_at_nth_num)
             ~f:(fun i -> start_at_nth_num + i + 1))
          ~init:start_value
          ~f:(fun acc next_idx ->
            let next_value = message.(next_idx) in
            let acc = acc + next_value in
            if acc > invalid_number
            then Stop None
            else if acc = invalid_number
            then (
              let contiguous_range =
                Array.slice message start_at_nth_num (next_idx + 1)
              in
              let compare = Int.compare in
              Stop
                (Some
                   ((Array.min_elt ~compare contiguous_range |> Option.value_exn)
                   + (Array.max_elt ~compare contiguous_range |> Option.value_exn))))
            else Continue acc)
          ~finish:(fun (_ : int) -> None))
    |> List.hd_exn
    |> print_int
;;

let solve = solve' ~preamble_length:25

let%expect_test _ =
  let input =
    {|35
 20
 15
 25
 47
 40
 62
 55
 65
 95
 102
 117
 150
 182
 127
 219
 299
 277
 309
 576
 6|}
    |> parse_as_input
  in
  let preamble_length = 5 in
  solve' ~preamble_length A input;
  let%bind () = [%expect {| 127 |}] in
  solve' ~preamble_length B input;
  [%expect {| 62 |}]
;;
