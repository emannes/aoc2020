open! Core
open! Async
open Common
include Solution

module Problem3 : S = struct
  let count file_contents (right, down) =
    List.chunks_of ~length:down file_contents
    |> List.map ~f:List.hd_exn
    |> List.counti ~f:(fun i row -> Char.equal '#' row.[right * i mod String.length row])
  ;;

  let solve subpart file_contents =
    match (subpart : Subpart.t) with
    | A -> count file_contents (3, 1) |> print_int
    | B ->
      List.map [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ] ~f:(count file_contents)
      |> List.reduce_exn ~f:( * )
      |> print_int
  ;;
end

module Problem4 : S = struct
  (* byr (Birth Year)
     iyr (Issue Year)
     eyr (Expiration Year)
     hgt (Height)
     hcl (Hair Color)
     ecl (Eye Color)
     pid (Passport ID)
     cid (Country ID)
  *)

  type t =
    { byr : int
    ; iyr : int
    ; eyr : int
    ; hgt : string
    ; hcl : string
    ; ecl : string
    ; pid : string
    }
  [@@deriving fields]

  let of_map map =
    let g field = Map.find_exn map field in
    let gi field = g field |> Int.of_string in
    Or_error.try_with (fun () ->
        Fields.create
          ~byr:(gi "byr")
          ~eyr:(gi "eyr")
          ~iyr:(gi "iyr")
          ~hgt:(g "hgt")
          ~hcl:(g "hcl")
          ~ecl:(g "ecl")
          ~pid:(g "pid"))
  ;;

  (* byr (Birth Year) - four digits; at least 1920 and at most 2002.
     iyr (Issue Year) - four digits; at least 2010 and at most 2020.
     eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
     hgt (Height) - a number followed by either cm or in:
     If cm, the number must be at least 150 and at most 193.
     If in, the number must be at least 59 and at most 76.
     hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
     ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
     pid (Passport ID) - a nine-digit number, including leading zeroes.
     cid (Country ID) - ignored, missing or not.
  *)

  let hgt_re =
    let open Re in
    alt
      [ seq
          [ alt [ str "59"; seq [ char '6'; digit ]; seq [ char '7'; rg '0' '6' ] ]
          ; str "in"
          ]
      ; seq
          [ char '1'
          ; alt [ seq [ rg '5' '8'; digit ]; seq [ char '9'; rg '0' '3' ] ]
          ; str "cm"
          ]
      ]
    |> whole_string
    |> Re.compile
  ;;

  let pid_re =
    let open Re in
    repn digit 9 (Some 9) |> whole_string |> compile
  ;;

  let hcl_re =
    let open Re in
    seq
      [ char '#'
      ; repn (alt (digit :: (String.to_list "abcdef" |> List.map ~f:char))) 6 (Some 6)
      ]
    |> whole_string
    |> Re.compile
  ;;

  let is_valid t =
    let btw low high field = Int.between ~low ~high (Field.get field t) in
    let _ign _field = true in
    let matches re field = Re.execp re (Field.get field t) in
    Fields.for_all
      ~byr:(btw 1920 2002)
      ~iyr:(btw 2010 2020)
      ~eyr:(btw 2020 2030)
      ~ecl:(fun field ->
        List.mem
          [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
          (Field.get field t)
          ~equal:String.equal)
      ~hgt:(matches hgt_re)
      ~pid:(matches pid_re)
      ~hcl:(matches hcl_re)
  ;;

  let parse file_contents =
    List.group file_contents ~break:(fun row1 row2 ->
        (not (String.is_empty row1)) && String.is_empty row2)
    |> List.map ~f:(fun lines ->
           String.concat lines ~sep:" "
           |> String.split ~on:' '
           |> List.filter ~f:(fun s -> not (String.is_empty s))
           |> List.map ~f:(String.rsplit2_exn ~on:':')
           |> String.Map.of_alist_exn)
  ;;

  let solve subpart file_contents =
    let passports = parse file_contents |> List.map ~f:of_map in
    match (subpart : Subpart.t) with
    | A -> List.count passports ~f:Or_error.is_ok |> print_int
    | B ->
      List.count passports ~f:(function
          | Ok passport -> is_valid passport
          | Error _ -> false)
      |> print_int
  ;;
end

module Problem15 : S = struct
  let nth_turn starting_numbers num_turns =
    let last_said_on =
      List.mapi starting_numbers ~f:(fun turn number -> number, turn + 1)
      |> Int.Map.of_alist_exn
    in
    let rec eval turns_so_far last_said last_said_on =
      if turns_so_far = num_turns
      then last_said
      else (
        let number_to_say =
          match Map.find last_said_on last_said with
          | None -> 0
          | Some turn -> turns_so_far - turn
        in
        eval
          (turns_so_far + 1)
          number_to_say
          (Map.set last_said_on ~key:last_said ~data:turns_so_far))
    in
    let last_said = List.last_exn starting_numbers in
    eval (List.length starting_numbers) last_said (Map.remove last_said_on last_said)
  ;;

  let%expect_test _ =
    List.iter [ 3; 4; 5; 6; 7; 8; 9; 10 ] ~f:(fun n ->
        nth_turn [ 0; 3; 6 ] n |> print_int);
    [%expect {|
      6
      0
      3
      3
      1
      0
      4
      0 |}]
  ;;

  let solve subpart _file_contents =
    let input = [ 0; 1; 4; 13; 15; 12; 16 ] in
    match (subpart : Subpart.t) with
    | A -> print_int (nth_turn input 2020)
    | B -> print_int (nth_turn input 30000000)
  ;;
end

module Problem17 : S = struct
  module Cube = struct
    module T = struct
      type t = int * int * int * int [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)

    let ( + ) (a1, a2, a3, a4) (b1, b2, b3, b4) = a1 + b1, a2 + b2, a3 + b3, a4 + b4

    let neighbor_offsets subpart : t list =
      let pm1 = [ -1; 0; 1 ] in
      List.cartesian_product pm1 pm1
      |> List.cartesian_product pm1
      |> List.cartesian_product
           (match (subpart : Subpart.t) with
           | A -> [ 0 ]
           | B -> pm1)
      |> List.filter_map ~f:(fun (d, (a, (b, c))) ->
             if List.exists ~f:Int.(( <> ) 0) [ a; b; c; d ]
             then Some (a, b, c, d)
             else None)
    ;;

    let neighbors subpart cube = List.map (neighbor_offsets subpart) ~f:(( + ) cube)
  end

  let step subpart (active_cubes : Cube.Set.t) =
    Set.to_list active_cubes
    |> List.concat_map ~f:(Cube.neighbors subpart)
    |> List.map ~f:(fun cube -> cube, 1)
    |> Cube.Map.of_alist_reduce ~f:( + )
    |> Map.filteri ~f:(fun ~key:cube ~data:active_neighbors ->
           if Set.mem active_cubes cube
           then active_neighbors = 2 || active_neighbors = 3
           else active_neighbors = 3)
    |> Map.key_set
  ;;

  let parse file_contents =
    List.map file_contents ~f:String.to_list
    |> List.concat_mapi ~f:(fun x cubes ->
           List.filter_mapi cubes ~f:(fun y cube ->
               if Char.equal cube '#' then Some (x, y, 0, 0) else None))
    |> Cube.Set.of_list
  ;;

  let solve subpart file_contents =
    let initial_state = parse file_contents in
    Fn.apply_n_times ~n:6 (step subpart) initial_state |> Set.length |> print_int
  ;;

  let%expect_test _ =
    let file_contents = {|.#.
  ..#
  ###|} |> parse_as_input in
    solve A file_contents;
    let%bind () = [%expect {| 112 |}] in
    solve B file_contents;
    let%bind () = [%expect {| 848 |}] in
    return ()
  ;;
end

module Problem18 : S = struct
  let solve subpart file_contents =
    let eval =
      match (subpart : Subpart.t) with
      | A -> Arithmetic.Expression.eval_equal_precedence
      | B -> Arithmetic.Expression.eval_addition_precedes_mult
    in
    List.sum
      (module Int)
      file_contents
      ~f:(fun line ->
        Arithmetic.parse_with_error (Lexing.from_string line) |> ok_exn |> eval)
    |> print_int
  ;;
end

let command =
  Command.async
    ~summary:"AOC"
    (let%map_open.Command () = return ()
     and input_file = anon ("input_file" %: string)
     and problem = anon ("problem-num" %: int)
     and subpart = anon ("subpart" %: sexp_conv Subpart.t_of_sexp) in
     fun () ->
       let (module M : S) =
         List.nth_exn
           [ (module Problem1 : S)
           ; (module Problem2 : S)
           ; (module Problem3 : S)
           ; (module Problem4 : S)
           ; (* Did problem 5 in bash *)
             (module Not_implemented : S)
           ; (module Problem6 : S)
           ; (module Problem7 : S)
           ; (module Problem8 : S)
           ; (module Problem9 : S)
           ; (module Problem10 : S)
           ; (module Problem11 : S)
           ; (module Problem12 : S)
           ; (module Problem13 : S)
           ; (module Problem14 : S)
           ; (module Problem15 : S)
           ; (module Problem16 : S)
           ; (module Problem17 : S)
           ; (module Problem18 : S)
           ; (module Problem19 : S)
           ; (module Problem20 : S)
           ]
           (problem - 1)
       in
       Reader.file_lines input_file >>| M.solve subpart)
;;
