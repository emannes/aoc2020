open! Core
open Async
open Common

module Instruction = struct
  module T = struct
    type t =
      | Acc
      | Jmp
      | Nop
    [@@deriving sexp, equal]
  end

  include T
  include Sexpable.To_stringable (T)
end

module State = struct
  type t =
    { pointer : int
    ; acc : int
    ; visited_pointers : Int.Set.t
    }

  let init = { pointer = 0; acc = 0; visited_pointers = Int.Set.empty }

  let step { pointer; acc; visited_pointers } ((instruction : Instruction.t), arg) =
    { pointer =
        (match instruction with
        | Acc | Nop -> pointer + 1
        | Jmp -> pointer + arg)
    ; acc =
        (match instruction with
        | Acc -> acc + arg
        | Nop | Jmp -> acc)
    ; visited_pointers = Set.add visited_pointers pointer
    }
  ;;
end

type t = (Instruction.t * int) array

let step t state = State.step state t.(state.pointer)

let run t =
  let rec run' (state : State.t) =
    if Set.mem state.visited_pointers state.pointer
    then `Loop, state.acc
    else if state.pointer = Array.length t
    then `Terminated, state.acc
    else if state.pointer > Array.length t
    then `Out_of_bounds, state.acc
    else run' (step t state)
  in
  run' State.init
;;

let parse file_contents : t =
  List.map file_contents ~f:(fun s ->
      let instruction, n = String.rsplit2_exn s ~on:' ' in
      Instruction.of_string instruction, Int.of_string n)
  |> Array.of_list
;;

let solve subpart file_contents =
  let program = parse file_contents in
  match (subpart : Subpart.t) with
  | A ->
    let end_state = run program in
    (match end_state with
    | `Loop, acc -> print_int acc
    | _ -> failwith "Unexpected end state")
  | B ->
    Array.find_mapi program ~f:(fun i (instruction, arg) ->
        let altered_program = Array.copy program in
        let altered_instruction =
          let open Instruction in
          match (instruction : Instruction.t) with
          | Nop -> Jmp
          | Jmp -> Nop
          | Acc -> Acc
        in
        altered_program.(i) <- altered_instruction, arg;
        if Instruction.equal instruction altered_instruction
        then None
        else (
          match run altered_program with
          | (`Loop | `Out_of_bounds), _ -> None
          | `Terminated, acc -> Some acc))
    |> Option.value_exn
    |> print_int
;;

let%expect_test _ =
  let input =
    {|nop +0
  acc +1
  jmp +4
  acc +3
  jmp -3
  acc -99
  acc +1
  jmp -4
  acc +6|}
    |> parse_as_input
  in
  solve A input;
  let%bind () = [%expect {| 5 |}] in
  solve B input;
  [%expect {| 8 |}]
;;
