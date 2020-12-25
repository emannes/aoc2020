open! Import

(* this is awful, I'm sorry *)

module Element = struct
  type t =
    { n : int
    ; mutable before : int option
    ; mutable after : int option
    }
  [@@deriving sexp]

  let of_int n = { n; before = None; after = None }
end

type t =
  { mutable element_map : Element.t Int.Map.t
  ; current : int
  }
[@@deriving sexp]

let of_list (ints : int list) =
  let elements = List.map ints ~f:(fun n -> n, Element.of_int n) in
  let element_map = Int.Map.of_alist_exn elements in
  let elements = List.map ~f:snd elements in
  let links = List.zip_with_remainder elements (List.tl_exn elements) |> fst in
  List.iter links ~f:(fun (element, next_element) ->
      element.after <- Some next_element.n;
      next_element.before <- Some element.n);
  let first, last = List.hd_exn elements, List.last_exn elements in
  first.before <- Some last.n;
  last.after <- Some first.n;
  { current = List.hd_exn ints; element_map }
;;

let remove_exn t n =
  let element = Map.find_exn t.element_map n in
  let before, after =
    ( element.before |> Option.value_exn |> Map.find_exn t.element_map
    , element.after |> Option.value_exn |> Map.find_exn t.element_map )
  in
  before.after <- Some after.n;
  after.before <- Some before.n;
  element.before <- None;
  element.after <- None;
  t.element_map <- Map.remove t.element_map n;
  ()
;;

let insert_exn t n ~just_after =
  let before = Map.find_exn t.element_map just_after in
  let after = before.after |> Option.value_exn |> Map.find_exn t.element_map in
  let element = Element.of_int n in
  before.after <- Some element.n;
  element.before <- Some before.n;
  element.after <- Some after.n;
  after.before <- Some element.n;
  t.element_map <- Map.set t.element_map ~key:n ~data:element;
  ()
;;

let after t n =
  let element_n = Map.find_exn t.element_map n in
  element_n.after |> Option.value_exn
;;
