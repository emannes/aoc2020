open! Core
open Async
open Common

module Cell_state = struct
  type t =
    | Floor
    | Empty_seat
    | Occupied
  [@@deriving equal]

  let of_char = function
    | '.' -> Floor
    | 'L' -> Empty_seat
    | '#' -> Occupied
    | _ -> failwith "not a valid character"
  ;;
end

(* x is the outer index, y is the inner index

i.e., x is the rows, y is the columns *)
type t = Cell_state.t array array

let parse file_contents =
  List.map file_contents ~f:(fun row ->
      String.to_list row |> List.map ~f:Cell_state.of_char |> List.to_array)
  |> List.to_array
;;

let dims t = Array.length t, Array.length t.(0)

let find (t : t) (x, y) =
  let xdim, ydim = dims t in
  if Int.between ~low:0 x ~high:(xdim - 1) && Int.between ~low:0 y ~high:(ydim - 1)
  then Some t.(x).(y)
  else None
;;

let rec first_seat_in_direction t (x, y) ~direction =
  let ox, oy = direction in
  let next_cell = x + ox, y + oy in
  match find t next_cell with
  | (Some (Occupied | Empty_seat) | None) as s -> s
  | Some Floor -> first_seat_in_direction t next_cell ~direction
;;

let all_coords t =
  let xdim, ydim = dims t in
  List.cartesian_product (List.init xdim ~f:Fn.id) (List.init ydim ~f:Fn.id)
;;

let equal t1 t2 =
  List.for_all (all_coords t1) ~f:(fun coords ->
      [%equal: Cell_state.t option] (find t1 coords) (find t2 coords))
;;

let deep_copy t =
  let copy = Array.copy t in
  Array.map_inplace copy ~f:Array.copy;
  copy
;;

let neighbor_offsets =
  List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
  (* i.e., everything but (0, 0) *)
  |> List.filter ~f:(fun (x, y) -> x <> 0 || y <> 0)
;;

let step_cell t (x, y) ~subpart : Cell_state.t =
  let occupied_neighbors =
    List.filter_map neighbor_offsets ~f:(fun (ox, oy) ->
        match (subpart : Subpart.t) with
        | A -> find t (x + ox, y + oy)
        | B -> first_seat_in_direction t (x, y) ~direction:(ox, oy))
    |> List.count ~f:(function
           | Occupied -> true
           | Empty_seat | Floor -> false)
  in
  match find t (x, y) with
  | Some Floor -> Floor
  | Some Empty_seat -> if occupied_neighbors = 0 then Occupied else Empty_seat
  | Some Occupied ->
    if occupied_neighbors
       >=
       match subpart with
       | A -> 4
       | B -> 5
    then Empty_seat
    else Occupied
  | None -> failwith "don't try to step a nonexistent cell"
;;

let step ~subpart t =
  let copy = deep_copy t in
  all_coords t |> List.iter ~f:(fun (x, y) -> copy.(x).(y) <- step_cell t (x, y) ~subpart);
  copy
;;

let rec stabilize ~subpart t =
  let stepped = step ~subpart t in
  if equal t stepped then t else stabilize ~subpart stepped
;;

let solve subpart file_contents =
  let initial_state = parse file_contents in
  let end_state = stabilize ~subpart initial_state in
  Array.sum (module Int) end_state ~f:(Array.count ~f:(Cell_state.equal Occupied))
  |> print_int
;;

let%expect_test _ =
  let input =
    {|L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL|}
    |> parse_as_input
  in
  solve A input;
  let%bind () = [%expect {| 37 |}] in
  solve B input;
  [%expect {| 26 |}]
;;
