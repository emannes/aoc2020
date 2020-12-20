open! Core
open Async
open Common

module Direction = struct
  type t =
    [ `N
    | `S
    | `E
    | `W
    ]
  [@@deriving sexp]

  (* going around CCW *)
  let to_int = function
    | `E -> 0
    | `N -> 1
    | `W -> 2
    | `S -> 3
  ;;

  let of_int n =
    match positive_mod n 4 with
    | 0 -> `E
    | 1 -> `N
    | 2 -> `W
    | 3 -> `S
    | _ -> failwith "can't happen"
  ;;

  let rotate t degrees = to_int t + (degrees / 90) |> of_int
end

module Action = struct
  module T = struct
    type t =
      [ Direction.t
      | `F
      | `L
      | `R
      ]
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)
end

module Vec = struct
  type t = int * int [@@deriving sexp]

  let ( + ) (a1, b1) (a2, b2) = a1 + a2, b1 + b2
  let scale n (a, b) = n * a, n * b

  let rotate (a, b) degrees =
    match positive_mod (degrees / 90) 4 with
    | 0 -> a, b
    | 1 -> -b, a
    | 2 -> -a, -b
    | 3 -> b, -a
    | _ -> failwith "can't happen"
  ;;

  let of_direction direction arg =
    match (direction : Direction.t) with
    | `N -> 0, arg
    | `S -> 0, -arg
    | `E -> arg, 0
    | `W -> -arg, 0
  ;;
end

module State = struct
  type t =
    { facing : Direction.t
    ; ship : Vec.t
    ; waypoint : Vec.t
    }
  [@@deriving sexp]

  let init = { facing = `E; ship = 0, 0; waypoint = 10, 1 }

  let distance t =
    let x, y = t.ship in
    abs x + abs y
  ;;

  let applyA t ((action : Action.t), (arg : int)) =
    let go direction = { t with ship = Vec.(t.ship + of_direction direction arg) } in
    match action with
    | `L -> { t with facing = Direction.rotate t.facing arg }
    | `R -> { t with facing = Direction.rotate t.facing (-arg) }
    | `F -> go t.facing
    | (`N | `S | `E | `W) as direction -> go direction
  ;;

  let applyB t (action, arg) =
    match action with
    | (`N | `S | `E | `W) as direction ->
      { t with waypoint = Vec.(t.waypoint + of_direction direction arg) }
    | `F -> { t with ship = Vec.(t.ship + scale arg t.waypoint) }
    | `L -> { t with waypoint = Vec.rotate t.waypoint arg }
    | `R -> { t with waypoint = Vec.rotate t.waypoint (-arg) }
  ;;
end

let solve subpart file_contents =
  let instructions =
    List.map file_contents ~f:(fun line ->
        let action, arg =
          List.split_n (String.to_list line) 1 |> Tuple.T2.map ~f:String.of_char_list
        in
        Action.of_string action, Int.of_string arg)
  in
  let end_state =
    let apply_instruction =
      match (subpart : Subpart.t) with
      | A -> State.applyA
      | B -> State.applyB
    in
    List.fold instructions ~init:State.init ~f:apply_instruction
  in
  State.distance end_state |> print_int
;;

let%expect_test _ =
  let input = {|F10
  N3
  F7
  R90
  F11|} |> parse_as_input in
  solve A input;
  let%bind () = [%expect {| 25 |}] in
  solve B input;
  [%expect {| 286 |}]
;;
