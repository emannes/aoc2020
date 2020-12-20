open! Core
open Common

module Re2 = struct
  include Re2

  let seq regexes = List.map regexes ~f:Re2.to_string |> String.concat |> Re2.of_string

  let alt regexes =
    "("
    ^ String.concat
        ~sep:"|"
        (List.map regexes ~f:Re2.to_string |> List.dedup_and_sort ~compare:String.compare)
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
          List.map subrules ~f:(Map.find_exn rules) |> List.map ~f:to_regex |> Option.all
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
