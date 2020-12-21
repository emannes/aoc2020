open! Core
open Common

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
  split_into_groups file_contents
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
