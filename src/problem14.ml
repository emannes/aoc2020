open! Core
open Async
open Common

module State = struct
  type t =
    { or_mask : int
    ; and_mask : int
    ; memory : int Int.Map.t
    }

  let init = { or_mask = 0; and_mask = (2 lsl 36) - 1; memory = Int.Map.empty }

  let insert { or_mask; and_mask; memory } ~address ~data =
    { or_mask
    ; and_mask
    ; memory = Map.set memory ~key:address ~data:(data lor or_mask land and_mask)
    }
  ;;
end

let to_or_n_and_masks mask =
  let to_int s = Int.of_string ("0b" ^ s) in
  let or_mask = String.tr mask ~target:'X' ~replacement:'0' |> to_int in
  let and_mask = String.tr mask ~target:'X' ~replacement:'1' |> to_int in
  or_mask, and_mask
;;

let expand_wildcards mask =
  let num_xs = String.count mask ~f:(Char.( = ) 'X') in
  let mask = String.tr ~target:'0' ~replacement:'Z' mask in
  let rec fill mask bits xs_left =
    if xs_left = 0
    then mask
    else
      fill
        (String.substr_replace_first
           mask
           ~pattern:"X"
           ~with_:(bits mod 2 |> Int.to_string))
        (bits lsr 1)
        (xs_left - 1)
  in
  List.init (Int.pow 2 num_xs) ~f:(fun bits ->
      fill mask bits num_xs |> String.tr ~target:'Z' ~replacement:'X')
;;

let%expect_test "expand_wildcards" =
  List.iter ~f:(printf "%s\n") (expand_wildcards "000000000000000000000000000000X1001X");
  [%expect
    {|
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX01XX10
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX11XX10
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX01XX11
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX11XX11 |}]
;;

let to_address_n_data_exn instruction =
  let open Re in
  let re =
    seq [ str "mem["; group (rep digit); str "] = "; group (rep digit) ]
    |> whole_string
    |> compile
  in
  let group = exec re instruction in
  Int.of_string (Group.get group 1), Int.of_string (Group.get group 2)
;;

let%expect_test "to_address_n_data" =
  to_address_n_data_exn "mem[48514] = 171994" |> [%sexp_of: int * int] |> print_s;
  [%expect {| (48514 171994) |}]
;;

(* a pretty ugly solution, but i'm tired *)
let solve subpart file_contents =
  match (subpart : Subpart.t) with
  | A ->
    let end_state =
      List.fold ~init:State.init file_contents ~f:(fun state instruction ->
          match String.chop_prefix instruction ~prefix:"mask = " with
          | Some mask ->
            let or_mask, and_mask = to_or_n_and_masks mask in
            { state with or_mask; and_mask }
          | None ->
            let address, data = to_address_n_data_exn instruction in
            State.insert state ~address ~data)
    in
    print_int (List.sum (module Int) (Map.data end_state.memory) ~f:Fn.id)
  | B ->
    List.fold
      file_contents
      ~init:(Int.Map.empty, [])
      ~f:(fun (memory, masks) instruction ->
        match String.chop_prefix instruction ~prefix:"mask = " with
        | Some mask -> memory, expand_wildcards mask
        | None ->
          let address, data = to_address_n_data_exn instruction in
          ( List.fold ~init:memory masks ~f:(fun memory mask ->
                let or_mask, and_mask = to_or_n_and_masks mask in
                Map.set memory ~key:(address lor or_mask land and_mask) ~data)
          , masks ))
    |> fst
    |> Map.data
    |> List.sum (module Int) ~f:Fn.id
    |> print_int
;;

let%expect_test _ =
  let input =
    {|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
  mem[8] = 11
  mem[7] = 101
  mem[8] = 0|}
    |> parse_as_input
  in
  solve A input;
  let%bind () = [%expect {| 165 |}] in
  return ()
;;
