open! Core
open Async
open Common

module Bus = struct
  type t = int option

  let of_string s : t = if String.( = ) s "x" then None else Some (Int.of_string s)
end

let multiplicative_inverse k ~mod_ =
  if k mod mod_ = 0
  then None
  else (
    let rec search_from i i_times_k =
      if i_times_k = 1 then i else search_from (i + 1) ((i_times_k + k) mod mod_)
    in
    Some (search_from 1 (k mod mod_)))
;;

let%expect_test "multiplicative_inverse" =
  List.iter [ 5, 17; 0, 17; 17, 17 ] ~f:(fun (k, mod_) ->
      printf !"%{sexp: int option}\n" (multiplicative_inverse k ~mod_));
  [%expect {|
    (7)
    ()
    () |}]
;;

let solve subpart file_contents =
  let earliest_departure_time, buses =
    match file_contents with
    | [ earliest_departure_time; buses ] ->
      ( Int.of_string earliest_departure_time
      , String.split buses ~on:',' |> List.map ~f:Bus.of_string )
    | _ -> failwith "can't parse"
  in
  match (subpart : Subpart.t) with
  | A ->
    let frequency, wait_time =
      List.filter_opt buses
      |> List.map ~f:(fun frequency ->
             frequency - (earliest_departure_time mod frequency), frequency)
      |> Int.Map.of_alist_exn
      |> Map.min_elt_exn
    in
    print_int (frequency * wait_time)
  | B ->
    let residues =
      List.filter_mapi buses ~f:(fun k -> function
        | Some n -> Some (n, positive_mod (-k) n)
        | None -> None)
    in
    let product = List.map ~f:fst residues |> List.reduce_exn ~f:( * ) in
    let answer =
      List.sum
        (module Int)
        residues
        ~f:(fun (n, k) ->
          let product_of_other_moduli = product / n in
          let addthis =
            k
            * product_of_other_moduli
            * Option.value_exn (multiplicative_inverse product_of_other_moduli ~mod_:n)
          in
          addthis)
    in
    print_int (answer mod product)
;;

let%expect_test _ =
  let input = {|939
7,13,x,x,59,x,31,19|} |> parse_as_input in
  solve A input;
  let%bind () = [%expect {| 295 |}] in
  solve B input;
  [%expect {| 1068781 |}]
;;
