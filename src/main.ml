open! Core
open! Async

let print_int n = printf "%d\n" n

let parse_as_input s = String.split_lines s |> List.map ~f:String.strip

module Subpart = struct
  type t = A | B [@@deriving sexp]
end

module type S = sig
  val solve : Subpart.t -> string List.t -> unit
end

module Problem1 : S = struct
  let pairsumsto set target =
    Set.filter set ~f:(fun i -> Set.mem set (target - i))
    |> Set.to_list |> List.hd

  let solve subpart file_contents =
    let expenses = List.map file_contents ~f:Int.of_string |> Int.Set.of_list in
    match (subpart : Subpart.t) with
    | A ->
        pairsumsto expenses 2020 |> Option.value_exn |> fun i ->
        print_int (i * (2020 - i))
    | B ->
        Set.iter expenses ~f:(fun i ->
            match pairsumsto expenses (2020 - i) with
            | Some expense -> print_int (i * expense * (2020 - i - expense))
            | None -> ())
end

module Problem2 : S = struct
  type t = { bounds : int * int; requirement : Char.t; password : string }

  let re =
    let open Re in
    seq
      [
        group (rep digit);
        char '-';
        group (rep digit);
        char ' ';
        group alpha;
        str ": ";
        group (rep notnl);
      ]
    |> whole_string |> compile

  let of_string s =
    let open Re in
    let groups = exec re s |> Group.all in
    {
      bounds = (groups.(1) |> Int.of_string, groups.(2) |> Int.of_string);
      requirement = groups.(3) |> Char.of_string;
      password = groups.(4);
    }

  let is_satisfied_a { bounds = low, high; requirement; password } =
    let occurrences =
      String.to_list password
      |> List.filter ~f:(Char.equal requirement)
      |> List.length
    in
    Int.between ~low ~high occurrences

  let is_satisfied_b { bounds = pos1, pos2; requirement; password } =
    let safe_get n =
      if String.length password < n then None else Some password.[n - 1]
    in
    List.map ~f:safe_get [ pos1; pos2 ]
    |> List.filter_opt
    |> List.count ~f:(Char.equal requirement)
    |> ( = ) 1

  let solve subpart file_contents =
    let ts = List.map file_contents ~f:of_string in
    match (subpart : Subpart.t) with
    | A -> List.filter ts ~f:is_satisfied_a |> List.length |> print_int
    | B -> List.filter ts ~f:is_satisfied_b |> List.length |> print_int
end

module Problem3 : S = struct
  let count file_contents (right, down) =
    List.chunks_of ~length:down file_contents
    |> List.map ~f:List.hd_exn
    |> List.counti ~f:(fun i row ->
           Char.equal '#' row.[right * i mod String.length row])

  let solve subpart file_contents =
    match (subpart : Subpart.t) with
    | A -> count file_contents (3, 1) |> print_int
    | B ->
        List.map
          [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
          ~f:(count file_contents)
        |> List.reduce_exn ~f:( * ) |> print_int
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

  type t = {
    byr : int;
    iyr : int;
    eyr : int;
    hgt : string;
    hcl : string;
    ecl : string;
    pid : string;
  }
  [@@deriving fields]

  let of_map map =
    let g field = Map.find_exn map field in
    let gi field = g field |> Int.of_string in
    Or_error.try_with (fun () ->
        Fields.create ~byr:(gi "byr") ~eyr:(gi "eyr") ~iyr:(gi "iyr")
          ~hgt:(g "hgt") ~hcl:(g "hcl") ~ecl:(g "ecl") ~pid:(g "pid"))

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
      [
        seq
          [
            alt
              [
                str "59"; seq [ char '6'; digit ]; seq [ char '7'; rg '0' '6' ];
              ];
            str "in";
          ];
        seq
          [
            char '1';
            alt [ seq [ rg '5' '8'; digit ]; seq [ char '9'; rg '0' '3' ] ];
            str "cm";
          ];
      ]
    |> whole_string |> Re.compile

  let pid_re =
    let open Re in
    repn digit 9 (Some 9) |> whole_string |> compile

  let hcl_re =
    let open Re in
    seq
      [
        char '#';
        repn
          (alt (digit :: (String.to_list "abcdef" |> List.map ~f:char)))
          6 (Some 6);
      ]
    |> whole_string |> Re.compile

  let is_valid t =
    let btw low high field = Int.between ~low ~high (Field.get field t) in
    let _ign _field = true in
    let matches re field = Re.execp re (Field.get field t) in
    Fields.for_all ~byr:(btw 1920 2002) ~iyr:(btw 2010 2020)
      ~eyr:(btw 2020 2030)
      ~ecl:(fun field ->
        List.mem
          [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
          (Field.get field t) ~equal:String.equal)
      ~hgt:(matches hgt_re) ~pid:(matches pid_re) ~hcl:(matches hcl_re)

  let parse file_contents =
    List.group file_contents ~break:(fun row1 row2 ->
        (not (String.is_empty row1)) && String.is_empty row2)
    |> List.map ~f:(fun lines ->
           String.concat lines ~sep:" "
           |> String.split ~on:' '
           |> List.filter ~f:(fun s -> not (String.is_empty s))
           |> List.map ~f:(String.rsplit2_exn ~on:':')
           |> String.Map.of_alist_exn)

  let solve subpart file_contents =
    let passports = parse file_contents |> List.map ~f:of_map in
    match (subpart : Subpart.t) with
    | A -> List.count passports ~f:Or_error.is_ok |> print_int
    | B ->
        List.count passports ~f:(function
          | Ok passport -> is_valid passport
          | Error _ -> false)
        |> print_int
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

  let%expect_test _ =
    solve B
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
end

module Problem7 : S = struct
  let parse file_contents =
    List.map file_contents ~f:(fun line ->
        match
          String.chop_suffix line ~suffix:" bags contain no other bags."
        with
        | Some bag -> (bag, [])
        | None -> (
            match String.split ~on:' ' line with
            | attr :: color :: "bags" :: "contain" :: rest ->
                ( attr ^ " " ^ color,
                  List.chunks_of rest ~length:4
                  |> List.map ~f:(fun l ->
                         ( Int.of_string (List.nth_exn l 0),
                           List.nth_exn l 1 ^ " " ^ List.nth_exn l 2 )) )
            | _ -> failwith "parse error" ))

  let can_have_parent rules =
    List.concat_map rules ~f:(fun (container, contained) ->
        List.map contained ~f:(fun (_n, contained) -> (contained, container)))
    |> String.Map.of_alist_multi

  let can_have_ancestor rules bag =
    let can_have_parent = can_have_parent rules in
    let rec can_have_ancestor' checked to_check =
      if Set.is_empty to_check then checked
      else
        let parents =
          Set.to_list to_check
          |> List.concat_map ~f:(fun bag ->
                 Map.find can_have_parent bag |> Option.value ~default:[])
          |> String.Set.of_list
        in
        can_have_ancestor' (Set.union checked to_check) parents
    in
    can_have_ancestor' String.Set.empty (String.Set.singleton bag)

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

  let solve subpart file_contents =
    let rules = parse file_contents in
    match (subpart : Subpart.t) with
    | A -> Set.length (can_have_ancestor rules "shiny gold") - 1 |> print_int
    | B ->
        num_descendants (String.Map.of_alist_exn rules) "shiny gold" - 1
        |> print_int

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

  let%expect_test "A" =
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
end

module Problem8 : S = struct
  module Instruction = struct
    module T = struct
      type t = Acc | Jmp | Nop [@@deriving sexp, equal]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  module State = struct
    type t = { pointer : int; acc : int; visited_pointers : Int.Set.t }

    let init = { pointer = 0; acc = 0; visited_pointers = Int.Set.empty }

    let step { pointer; acc; visited_pointers }
        ((instruction : Instruction.t), arg) =
      {
        pointer =
          ( match instruction with
          | Acc | Nop -> pointer + 1
          | Jmp -> pointer + arg );
        acc = (match instruction with Acc -> acc + arg | Nop | Jmp -> acc);
        visited_pointers = Set.add visited_pointers pointer;
      }
  end

  type t = (Instruction.t * int) array

  let step t state = State.step state t.(state.pointer)

  let run t =
    let rec run' (state : State.t) =
      if Set.mem state.visited_pointers state.pointer then (`Loop, state.acc)
      else if state.pointer = Array.length t then (`Terminated, state.acc)
      else if state.pointer > Array.length t then (`Out_of_bounds, state.acc)
      else run' (step t state)
    in
    run' State.init

  let parse file_contents : t =
    List.map file_contents ~f:(fun s ->
        let instruction, n = String.rsplit2_exn s ~on:' ' in
        (Instruction.of_string instruction, Int.of_string n))
    |> Array.of_list

  let solve subpart file_contents =
    let program = parse file_contents in
    match (subpart : Subpart.t) with
    | A -> (
        let end_state = run program in
        match end_state with
        | `Loop, acc -> print_int acc
        | _ -> failwith "Unexpected end state" )
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
            altered_program.(i) <- (altered_instruction, arg);
            if Instruction.equal instruction altered_instruction then None
            else
              match run altered_program with
              | (`Loop | `Out_of_bounds), _ -> None
              | `Terminated, acc -> Some acc)
        |> Option.value_exn |> print_int

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
end

module Problem9 : S = struct
  (* The data in [recents] is the # of occurrences in the last [preamble_length] *)
  type t = {
    preamble_length : int;
    trailing : int array;
    idx_of_oldest : int ref;
    frequencies : int Int.Table.t;
    num_ints_processed : int ref;
  }
  [@@deriving sexp]

  open Continue_or_stop

  let nth_oldest t n =
    if Int.between n ~low:0 ~high:t.preamble_length then
      t.trailing.((!(t.idx_of_oldest) + n) mod t.preamble_length)
    else failwith "out of bounds"

  let init ~preamble_length message =
    let preamble, rest = List.split_n message preamble_length in
    let frequencies =
      List.map preamble ~f:(fun i -> (i, 1))
      |> Int.Map.of_alist_reduce ~f:( + )
      |> Map.to_alist |> Int.Table.of_alist_exn
    in
    ( {
        preamble_length;
        trailing = Array.of_list preamble;
        idx_of_oldest = ref 0;
        frequencies;
        num_ints_processed = ref preamble_length;
      },
      rest )

  let step
      ( {
          preamble_length;
          trailing;
          idx_of_oldest;
          frequencies;
          num_ints_processed;
        } as t ) next_value =
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
          | Some frequency ->
              if frequency > 1 then Some (frequency - 1) else None);
        Hashtbl.update frequencies next_value ~f:(function
          | None -> 1
          | Some frequency -> frequency + 1);
        Continue ()

  let solve' ~preamble_length subpart file_contents =
    let message = List.map ~f:Int.of_string file_contents in
    let t, rest = init ~preamble_length message in
    let invalid_number =
      List.fold_until rest ~init:()
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
                 (num_ints_processed - start_at_nth_num) ~f:(fun i ->
                   start_at_nth_num + i + 1))
              ~init:start_value
              ~f:(fun acc next_idx ->
                let next_value = message.(next_idx) in
                let acc = acc + next_value in
                if acc > invalid_number then Stop None
                else if acc = invalid_number then
                  let contiguous_range =
                    Array.slice message start_at_nth_num (next_idx + 1)
                  in
                  let compare = Int.compare in
                  Stop
                    (Some
                       ( ( Array.min_elt ~compare contiguous_range
                         |> Option.value_exn )
                       + ( Array.max_elt ~compare contiguous_range
                         |> Option.value_exn ) ))
                else Continue acc)
              ~finish:(fun (_ : int) -> None))
        |> List.hd_exn |> print_int

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
end

module Problem10 : S = struct
  let solve subpart file_contents =
    let adapters =
      let in_the_bag =
        List.map file_contents ~f:Int.of_string |> Int.Set.of_list
      in
      Set.add in_the_bag (3 + Set.max_elt_exn in_the_bag)
    in
    match (subpart : Subpart.t) with
    | A ->
        let joltage_sequence =
          Set.to_list (Set.add adapters 0)
          (* for the outlet *)
        in
        let jumps =
          List.zip_with_remainder joltage_sequence
            (List.tl_exn joltage_sequence)
          |> fst
          |> List.map ~f:(fun (smaller, larger) -> larger - smaller)
        in
        let ones, threes =
          (List.count jumps ~f:(( = ) 1), List.count jumps ~f:(( = ) 3))
        in
        print_int (ones * threes)
    | B ->
        Set.fold adapters ~init:(Int.Map.singleton 0 1)
          ~f:(fun num_ways adapter ->
            Map.add_exn num_ways ~key:adapter
              ~data:
                ( Map.filter_keys num_ways
                    ~f:(Int.between ~low:(adapter - 3) ~high:(adapter - 1))
                |> Map.data |> List.reduce_exn ~f:( + ) ))
        |> Map.max_elt_exn |> snd |> print_int

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
end

module Not_implemented : S = struct
  let solve subpart _file_contents =
    match (subpart : Subpart.t) with
    | A -> failwith "not implemented"
    | B -> failwith "not implemented"
end

let command =
  Command.async ~summary:"AOC"
    (let%map_open.Command () = return ()
     and input_file = anon ("input_file" %: string)
     and problem = anon ("problem-num" %: int)
     and subpart = anon ("subpart" %: sexp_conv Subpart.t_of_sexp) in
     fun () ->
       let (module M : S) =
         List.nth_exn
           [
             (module Problem1 : S);
             (module Problem2 : S);
             (module Problem3 : S);
             (module Problem4 : S);
             (* Did problem 5 in bash *)
             (module Not_implemented : S);
             (module Problem6 : S);
             (module Problem7 : S);
             (module Problem8 : S);
             (module Problem9 : S);
             (module Problem10 : S);
           ]
           (problem - 1)
       in
       Reader.file_lines input_file >>| M.solve subpart)
