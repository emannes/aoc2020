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

module Problem6 : S = struct
  let solve subpart file_contents =
    let groups =
      List.map ~f:String.strip file_contents
      |> List.group ~break:(fun row1 row2 ->
             (not (String.is_empty row1)) && String.is_empty row2)
      |> List.filter ~f:(fun l -> not (List.is_empty l))
      |> List.map ~f:(List.filter ~f:(fun s -> not (String.is_empty s)))
    in
    match (subpart : Subpart.t) with
    | A ->
      List.sum
        (module Int)
        groups
        ~f:(fun qs ->
          String.concat qs |> String.to_list |> Char.Set.of_list |> Set.length)
      |> print_int
    | B ->
      List.sum
        (module Int)
        groups
        ~f:(fun qs ->
          let qs = List.filter qs ~f:(fun s -> not @@ String.is_empty s) in
          let num_people = List.length qs in
          List.sort (String.concat qs |> String.to_list) ~compare:Char.compare
          |> List.group ~break:Char.( <> )
          |> List.count ~f:(fun answers -> List.length answers = num_people))
      |> print_int
  ;;

  let%expect_test _ =
    solve
      B
      (parse_as_input
         {|abc

        a
        b
        c
        
        ab
        ac
        
        a
        a
        a
        a
        
        b|});
    [%expect {|
      6 |}]
  ;;
end

module Problem8 : S = struct
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
end

module Problem10 : S = struct
  let solve subpart file_contents =
    let adapters =
      let in_the_bag = List.map file_contents ~f:Int.of_string |> Int.Set.of_list in
      Set.add in_the_bag (3 + Set.max_elt_exn in_the_bag)
    in
    match (subpart : Subpart.t) with
    | A ->
      let joltage_sequence = Set.to_list (Set.add adapters 0) (* for the outlet *) in
      let jumps =
        List.zip_with_remainder joltage_sequence (List.tl_exn joltage_sequence)
        |> fst
        |> List.map ~f:(fun (smaller, larger) -> larger - smaller)
      in
      let ones, threes = List.count jumps ~f:(( = ) 1), List.count jumps ~f:(( = ) 3) in
      print_int (ones * threes)
    | B ->
      Set.fold adapters ~init:(Int.Map.singleton 0 1) ~f:(fun num_ways adapter ->
          Map.add_exn
            num_ways
            ~key:adapter
            ~data:
              (Map.filter_keys
                 num_ways
                 ~f:(Int.between ~low:(adapter - 3) ~high:(adapter - 1))
              |> Map.data
              |> List.reduce_exn ~f:( + )))
      |> Map.max_elt_exn
      |> snd
      |> print_int
  ;;

  let%expect_test "small" =
    let input =
      {|16
      10
      15
      5
      1
      11
      7
      19
      6
      12
      4|}
      |> parse_as_input
    in
    solve A input;
    [%expect {| 35 |}]
  ;;

  let%expect_test "large" =
    let input =
      {|28
      33
      18
      42
      31
      14
      46
      20
      48
      47
      24
      23
      49
      45
      19
      38
      39
      11
      1
      32
      25
      35
      8
      17
      7
      9
      4
      2
      34
      10
      3|}
      |> parse_as_input
    in
    solve A input;
    let%bind () = [%expect {| 220 |}] in
    solve B input;
    [%expect {| 19208 |}]
  ;;
end

module Problem11 : S = struct
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
    all_coords t
    |> List.iter ~f:(fun (x, y) -> copy.(x).(y) <- step_cell t (x, y) ~subpart);
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
end

module Problem13 : S = struct
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
end

module Problem14 : S = struct
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
