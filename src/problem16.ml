open! Core
open Async
open Common

module Input = struct
  type t =
    { allowed_ranges : Interval.Int.t list String.Map.t
    ; my_ticket : int list
    ; nearby_tickets : int list list
    }
  [@@deriving sexp]

  let parse_ticket ticket = String.split ticket ~on:',' |> List.map ~f:Int.of_string

  let parse_range line =
    let open Re in
    let re =
      let int_group = group (rep digit) in
      seq
        [ group (rep any)
        ; str ": "
        ; int_group
        ; char '-'
        ; int_group
        ; str " or "
        ; int_group
        ; char '-'
        ; int_group
        ]
      |> whole_string
      |> compile
    in
    let group = Re.exec re line in
    let geti n = Int.of_string (Group.get group n) in
    ( Group.get group 1
    , [ Interval.Int.create (geti 2) (geti 3); Interval.Int.create (geti 4) (geti 5) ] )
  ;;

  let of_contents file_contents =
    let allowed_ranges, my_ticket, nearby_tickets =
      match split_into_groups file_contents with
      | [ allowed_ranges
        ; ("your ticket:" :: my_ticket)
        ; ("nearby tickets:" :: nearby_tickets)
        ] ->
        ( List.map ~f:parse_range allowed_ranges |> String.Map.of_alist_exn
        , parse_ticket (List.hd_exn my_ticket)
        , List.map ~f:parse_ticket nearby_tickets )
      | _ -> failwith "couldn't parse "
    in
    { allowed_ranges; my_ticket; nearby_tickets }
  ;;

  let%expect_test "of_contents" =
    {|class: 1-3 or 5-7
  row: 6-11 or 33-44
  seat: 13-40 or 45-50
  
  your ticket:
  7,1,14
  
  nearby tickets:
  7,3,47
  40,4,50
  55,2,20
  38,6,12|}
    |> parse_as_input
    |> of_contents
    |> sexp_of_t
    |> print_s;
    [%expect
      {|
      ((allowed_ranges
        ((class ((1 3) (5 7))) (row ((6 11) (33 44))) (seat ((13 40) (45 50)))))
       (my_ticket (7 1 14))
       (nearby_tickets ((7 3 47) (40 4 50) (55 2 20) (38 6 12)))) |}]
  ;;
end

let solve subpart file_contents =
  let { Input.allowed_ranges; my_ticket; nearby_tickets } =
    Input.of_contents file_contents
  in
  let field_is_invalid n =
    List.for_all
      (Map.data allowed_ranges |> List.concat)
      ~f:(fun range -> not (Interval.Int.contains range n))
  in
  match (subpart : Subpart.t) with
  | A ->
    List.filter (List.concat nearby_tickets) ~f:field_is_invalid
    |> List.sum (module Int) ~f:Fn.id
    |> print_int
  | B ->
    let fields =
      let valid_tickets =
        List.filter nearby_tickets ~f:(fun ticket ->
            not (List.exists ticket ~f:field_is_invalid))
      in
      let (potential_fields : string list list) =
        List.transpose_exn valid_tickets
        |> List.map ~f:(fun field_values ->
               Map.filteri allowed_ranges ~f:(fun ~key:_ ~data:allowed_ranges ->
                   List.for_all field_values ~f:(fun value ->
                       List.exists allowed_ranges ~f:(fun range ->
                           Interval.Int.contains range value)))
               |> Map.keys)
      in
      (* This part only works because the elements of [potential_fields], 
      when sorted by size, are apparently a list of nested subsets  *)
      let by_num_potential_fields =
        List.map potential_fields ~f:(fun fields ->
            List.length fields, String.Set.of_list fields)
        |> Int.Map.of_alist_exn
      in
      let by_num_potential_fields =
        Map.mapi by_num_potential_fields ~f:(fun ~key ~data ->
            if key = 1
            then Set.min_elt_exn data
            else
              Set.diff data (Map.find_exn by_num_potential_fields (key - 1))
              |> Set.min_elt_exn)
      in
      List.map potential_fields ~f:(fun fields ->
          Map.find_exn by_num_potential_fields (List.length fields))
    in
    List.zip_exn fields my_ticket
    |> List.filter ~f:(fun (field, _) -> String.is_prefix field ~prefix:"departure")
    |> List.map ~f:snd
    |> List.reduce_exn ~f:( * )
    |> print_int
;;
