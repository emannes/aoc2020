open! Core
open Common

module Hex_dir = struct
  type t =
    | E
    | SE
    | SW
    | W
    | NW
    | NE
  [@@deriving sexp]

  (* x-axis = west-east, y-axis = southwest-northeast *)
  let to_pair = function
    | E -> 1, 0
    | W -> -1, 0
    | NE -> 0, 1
    | SW -> 0, -1
    | NW -> -1, 1
    | SE -> 1, -1
  ;;

  let of_string s : t list =
    let rec of_string' ts_rev remaining_chars =
      match remaining_chars with
      | [] -> List.rev ts_rev
      | 'e' :: remaining_chars -> of_string' (E :: ts_rev) remaining_chars
      | 's' :: 'e' :: remaining_chars -> of_string' (SE :: ts_rev) remaining_chars
      | 's' :: 'w' :: remaining_chars -> of_string' (SW :: ts_rev) remaining_chars
      | 'w' :: remaining_chars -> of_string' (W :: ts_rev) remaining_chars
      | 'n' :: 'e' :: remaining_chars -> of_string' (NE :: ts_rev) remaining_chars
      | 'n' :: 'w' :: remaining_chars -> of_string' (NW :: ts_rev) remaining_chars
      | _ -> failwith "parse error"
    in
    of_string' [] (String.to_list s)
  ;;

  let%expect_test "of_string" =
    print_s [%message (of_string "esew" : t list)];
    print_s [%message (of_string "nwwswee" : t list)];
    [%expect
      {|
      ("of_string \"esew\"" (E SE W))
      ("of_string \"nwwswee\"" (NW W SW E E)) |}]
  ;;
end

let solve subpart file_contents =
  let starting_cells =
    List.map file_contents ~f:Hex_dir.of_string
    |> List.map ~f:(List.sum (module Pair) ~f:Hex_dir.to_pair)
    |> List.map ~f:(fun cell -> cell, 1)
    |> Pair.Map.of_alist_reduce ~f:( + )
    |> Map.filter ~f:(fun frequency -> frequency mod 2 = 1)
  in
  match (subpart : Subpart.t) with
  | A -> Map.length starting_cells |> print_int
  | B -> failwith "not implemented"
;;

let%expect_test _ =
  let file_contents =
    {|sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew|}
    |> parse_as_input
  in
  solve A file_contents;
  [%expect {| 10 |}]
;;
