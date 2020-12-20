open! Core
open! Async
open Common
include Solution

module Problem1 : S = struct
  let pairsumsto set target =
    Set.filter set ~f:(fun i -> Set.mem set (target - i)) |> Set.to_list |> List.hd
  ;;

  let solve subpart file_contents =
    let expenses = List.map file_contents ~f:Int.of_string |> Int.Set.of_list in
    match (subpart : Subpart.t) with
    | A ->
      pairsumsto expenses 2020 |> Option.value_exn |> fun i -> print_int (i * (2020 - i))
    | B ->
      Set.iter expenses ~f:(fun i ->
          match pairsumsto expenses (2020 - i) with
          | Some expense -> print_int (i * expense * (2020 - i - expense))
          | None -> ())
  ;;
end

module Problem2 : S = struct
  type t =
    { bounds : int * int
    ; requirement : Char.t
    ; password : string
    }

  let re =
    let open Re in
    seq
      [ group (rep digit)
      ; char '-'
      ; group (rep digit)
      ; char ' '
      ; group alpha
      ; str ": "
      ; group (rep notnl)
      ]
    |> whole_string
    |> compile
  ;;

  let of_string s =
    let open Re in
    let groups = exec re s |> Group.all in
    { bounds = groups.(1) |> Int.of_string, groups.(2) |> Int.of_string
    ; requirement = groups.(3) |> Char.of_string
    ; password = groups.(4)
    }
  ;;

  let is_satisfied_a { bounds = low, high; requirement; password } =
    let occurrences =
      String.to_list password |> List.filter ~f:(Char.equal requirement) |> List.length
    in
    Int.between ~low ~high occurrences
  ;;

  let is_satisfied_b { bounds = pos1, pos2; requirement; password } =
    let safe_get n = if String.length password < n then None else Some password.[n - 1] in
    List.map ~f:safe_get [ pos1; pos2 ]
    |> List.filter_opt
    |> List.count ~f:(Char.equal requirement)
    |> ( = ) 1
  ;;

  let solve subpart file_contents =
    let ts = List.map file_contents ~f:of_string in
    match (subpart : Subpart.t) with
    | A -> List.filter ts ~f:is_satisfied_a |> List.length |> print_int
    | B -> List.filter ts ~f:is_satisfied_b |> List.length |> print_int
  ;;
end

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

module Problem7 : S = struct
  let parse file_contents =
    List.map file_contents ~f:(fun line ->
        match String.chop_suffix line ~suffix:" bags contain no other bags." with
        | Some bag -> bag, []
        | None ->
          (match String.split ~on:' ' line with
          | attr :: color :: "bags" :: "contain" :: rest ->
            ( attr ^ " " ^ color
            , List.chunks_of rest ~length:4
              |> List.map ~f:(fun l ->
                     ( Int.of_string (List.nth_exn l 0)
                     , List.nth_exn l 1 ^ " " ^ List.nth_exn l 2 )) )
          | _ -> failwith "parse error"))
  ;;

  let can_have_parent rules =
    List.concat_map rules ~f:(fun (container, contained) ->
        List.map contained ~f:(fun (_n, contained) -> contained, container))
    |> String.Map.of_alist_multi
  ;;

  let can_have_ancestor rules bag =
    let can_have_parent = can_have_parent rules in
    let rec can_have_ancestor' checked to_check =
      if Set.is_empty to_check
      then checked
      else (
        let parents =
          Set.to_list to_check
          |> List.concat_map ~f:(fun bag ->
                 Map.find can_have_parent bag |> Option.value ~default:[])
          |> String.Set.of_list
        in
        can_have_ancestor' (Set.union checked to_check) parents)
    in
    can_have_ancestor' String.Set.empty (String.Set.singleton bag)
  ;;

  (** includes self as a depth-0 descendant *)
  let rec num_descendants rules bag =
    match Map.find rules bag with
    | None -> 1
    | Some children ->
      1
      + List.sum
          (module Int)
          children
          ~f:(fun (n, child) -> n * num_descendants rules child)
  ;;

  let solve subpart file_contents =
    let rules = parse file_contents in
    match (subpart : Subpart.t) with
    | A -> Set.length (can_have_ancestor rules "shiny gold") - 1 |> print_int
    | B -> num_descendants (String.Map.of_alist_exn rules) "shiny gold" - 1 |> print_int
  ;;

  let%expect_test "A" =
    let input =
      {|light red bags contain 1 bright white bag, 2 muted yellow bags.
    dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    bright white bags contain 1 shiny gold bag.
    muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    faded blue bags contain no other bags.
    dotted black bags contain no other bags.|}
      |> parse_as_input
    in
    solve A input;
    [%expect {| 4 |}]
  ;;

  let%expect_test "B" =
    let input =
      {|shiny gold bags contain 2 dark red bags.
      dark red bags contain 2 dark orange bags.
      dark orange bags contain 2 dark yellow bags.
      dark yellow bags contain 2 dark green bags.
      dark green bags contain 2 dark blue bags.
      dark blue bags contain 2 dark violet bags.
      dark violet bags contain no other bags.|}
      |> parse_as_input
    in
    solve B input;
    [%expect {| 126 |}]
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

module Problem9 : S = struct
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

module Problem12 : S = struct
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

module Problem16 : S = struct
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
        match
          List.group file_contents ~break:(fun _line1 line2 -> String.is_empty line2)
        with
        | [ allowed_ranges
          ; ("" :: "your ticket:" :: my_ticket)
          ; ("" :: "nearby tickets:" :: nearby_tickets)
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

module Problem19 : S = struct
  module Re2 = struct
    include Re2

    let seq regexes = List.map regexes ~f:Re2.to_string |> String.concat |> Re2.of_string

    let alt regexes =
      "("
      ^ String.concat
          ~sep:"|"
          (List.map regexes ~f:Re2.to_string
          |> List.dedup_and_sort ~compare:String.compare)
      ^ ")"
      |> Re2.of_string
    ;;

    let repn regex n = List.init n ~f:(fun _ -> regex) |> seq
  end

  module Rule = struct
    type t =
      | Regex of Re2.t
      | Subrules of int list
      | Alt of t list
    [@@deriving sexp, compare]

    let of_string s =
      let s = String.split s ~on:' ' in
      let rule_num = Int.of_string (String.chop_suffix_exn ~suffix:":" (List.hd_exn s)) in
      let rule =
        match List.tl_exn s with
        | [ rest ] ->
          if String.is_prefix ~prefix:"\"" rest
          then
            Regex (List.nth_exn (String.to_list rest) 1 |> Char.to_string |> Re2.of_string)
          else Subrules [ Int.of_string rest ]
        | rest ->
          Alt
            (List.group rest ~break:(fun _word1 word2 -> String.equal word2 "|")
            |> List.map ~f:(fun subrules ->
                   Subrules
                     (List.filter subrules ~f:(fun word -> String.( <> ) word "|")
                     |> List.map ~f:Int.of_string)))
      in
      rule_num, rule
    ;;

    let to_regex = function
      | Regex r -> Some r
      | Alt _ | Subrules _ -> None
    ;;

    let simplify_once rules =
      let rec simplify = function
        | Regex r -> Regex r
        | Alt alts ->
          (match List.map alts ~f:simplify with
          | [] -> failwith "uh oh"
          | [ t ] -> t
          | ts ->
            let ts' = List.map ts ~f:to_regex |> Option.all in
            (match ts' with
            | Some regexes -> Regex (Re2.alt regexes)
            | None -> Alt ts))
        | Subrules subrules ->
          let subrules' =
            List.map subrules ~f:(Map.find_exn rules)
            |> List.map ~f:to_regex
            |> Option.all
          in
          (match subrules' with
          | Some regexes -> Regex (Re2.seq regexes)
          | None -> Subrules subrules)
      in
      let simplify_pair ~key ~data =
        match data with
        | Alt [ Regex r; Subrules [ other_rule; this_rule ] ] when key = this_rule ->
          (match to_regex (Map.find_exn rules other_rule) with
          | Some other_regex ->
            Regex
              (sprintf "(%s)*%s" (Re2.to_string other_regex) (Re2.to_string r)
              |> Re2.of_string)
          | None -> data)
        | Alt [ Regex r; Subrules [ other_rule1; this_rule; other_rule2 ] ]
          when key = this_rule ->
          (match
             ( to_regex (Map.find_exn rules other_rule1)
             , to_regex (Map.find_exn rules other_rule2) )
           with
          | Some other_regex1, Some other_regex2 ->
            (* Hack: rather than matching
              (other_regex1 x n)r(other_regex2 x n)

              for all n, we pick some maximum n instead.
             *)
            let max_repns = 10 in
            let open Re2 in
            Regex
              (List.init max_repns ~f:(fun i ->
                   seq [ repn other_regex1 i; r; repn other_regex2 i ])
              |> alt)
          | _ -> data)
        | data -> simplify data
      in
      Map.mapi rules ~f:simplify_pair
    ;;

    let rec simplify rules =
      let simplified = simplify_once rules in
      (* Re2 doesn't derive [equal] so we have to use [compare] *)
      if [%compare: t Int.Map.t] rules simplified = 0 then rules else simplify simplified
    ;;
  end

  let solve subpart file_contents =
    let rules, messages =
      List.filter file_contents ~f:(fun line -> not (String.is_empty line))
      |> List.split_while ~f:(fun line -> String.contains line ':')
    in
    let rules = List.map rules ~f:Rule.of_string |> Int.Map.of_alist_exn in
    let rules =
      match (subpart : Subpart.t) with
      | A -> rules
      | B ->
        Map.set rules ~key:8 ~data:(Alt [ Subrules [ 42 ]; Subrules [ 42; 8 ] ])
        |> Map.set ~key:11 ~data:(Alt [ Subrules [ 42; 31 ]; Subrules [ 42; 11; 31 ] ])
    in
    let re = Map.find_exn (Rule.simplify rules) 0 |> Rule.to_regex |> Option.value_exn in
    let re = sprintf "^%s$" (Re2.to_string re) |> Re2.of_string in
    List.filter messages ~f:(Re2.matches re) |> List.length |> print_int
  ;;

  let%expect_test _ =
    let file_contents =
      {|0: 4 1 5
  1: 2 3 | 3 2
  2: 4 4 | 5 5
  3: 4 5 | 5 4
  4: "a"
  5: "b"
  
  ababbb
  bababa
  abbbab
  aaabbb
  aaaabbb|}
      |> parse_as_input
    in
    solve A file_contents;
    [%expect {| 2 |}]
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
