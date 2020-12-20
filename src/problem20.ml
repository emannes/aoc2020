open! Core
open! Async
open Common

module Tile = struct
  type t = bool list list [@@deriving sexp]

  (* top/bottom read left to right, left/right read top-to-bottom *)
  let top = List.hd_exn
  let bottom = List.last_exn
  let left = List.map ~f:List.hd_exn
  let right = List.map ~f:List.last_exn

  let symmetries : (t -> t) list =
    (* Using standard dihedral group, D_8 (the symmetries of a square) *)
    let id = Fn.id in
    (* s is a reflection *)
    let s = List.rev in
    (* r is a rotation 90 degrees clockwise *)
    let rs = List.transpose_exn in
    let r = Fn.compose rs s in
    List.concat_map [ id; s ] ~f:(fun id_or_s ->
        List.init 4 ~f:(fun n -> Fn.apply_n_times ~n (Fn.compose r) id_or_s))
  ;;

  let of_lines lines : int * t =
    let hd, tl = List.hd_exn lines, List.tl_exn lines in
    let id =
      List.nth_exn (String.split_on_chars hd ~on:[ ' '; ':' ]) 1 |> Int.of_string
    in
    id, List.map tl ~f:(fun line -> List.map (String.to_list line) ~f:(Char.( = ) '#'))
  ;;

  let rows_equal = List.equal Bool.equal

  (* The orientation of [t1] is fixed, but [t2] is not *)
  let align (t1 : t) (t2 : t) : (t * [ `Under | `Right ]) option =
    let reoriented = List.map symmetries ~f:(fun g -> g t2) in
    List.filter_map reoriented ~f:(fun reoriented_t ->
        if rows_equal (bottom t1) (top reoriented_t)
        then Some (reoriented_t, `Under)
        else if rows_equal (right t1) (left reoriented_t)
        then Some (reoriented_t, `Right)
        else None)
    |> List.hd
  ;;

  let to_edges t = List.map [ top; bottom; left; right ] ~f:(fun e -> e t)

  let trim_border t =
    let size = List.length t in
    let trim_ends l = List.slice l 1 (size - 1) in
    List.map t ~f:trim_ends |> trim_ends
  ;;

  let concat_left_to_right (ts : t list) =
    List.transpose_exn ts |> List.map ~f:List.concat
  ;;

  let to_set t =
    List.concat_mapi t ~f:(fun i row ->
        List.filter_mapi row ~f:(fun j pixel -> if pixel then Some (i, j) else None))
    |> Pair.Set.of_list
  ;;
end

let edge_to_int (edge : bool list) =
  List.map [ edge; List.rev edge ] ~f:(fun edge ->
      List.mapi edge ~f:(fun i pixel -> if pixel then Int.pow 2 i else 0)
      |> List.reduce_exn ~f:( + ))
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let parse file_contents =
  List.group file_contents ~break:(fun _line1 line2 -> String.is_empty line2)
  |> List.map ~f:(List.filter ~f:(fun line -> not (String.is_empty line)))
  |> List.filter ~f:(fun lines -> List.length lines > 0)
  |> List.map ~f:Tile.of_lines
  |> Int.Map.of_alist_exn
;;

let invert (map : int list Int.Map.t) : int list Int.Map.t =
  Map.to_alist map
  |> List.concat_map ~f:(fun (key, data) -> List.map data ~f:(fun datum -> datum, key))
  |> Int.Map.of_alist_multi
;;

let sea_monster =
  "Tile 0:"
  :: ({|                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
|}
     |> String.tr ~target:' ' ~replacement:'.'
     |> parse_as_input)
  |> Tile.of_lines
  |> snd
;;

let solve subpart file_contents =
  let tiles = parse file_contents in
  let tiles_by_edge =
    Map.map tiles ~f:(fun tile -> Tile.to_edges tile |> List.map ~f:edge_to_int) |> invert
  in
  let unduped_edges = Map.filter tiles_by_edge ~f:(fun ids -> List.length ids = 1) in
  let unduped_edges_by_tile = invert unduped_edges in
  let corners =
    Map.filter unduped_edges_by_tile ~f:(fun unduped_edges ->
        List.length unduped_edges = 2)
    |> Map.keys
  in
  match (subpart : Subpart.t) with
  | A -> List.reduce_exn corners ~f:( * ) |> print_int
  | B ->
    let top_left_id = List.hd_exn corners in
    let top_left_tile =
      let unoriented = Map.find_exn tiles top_left_id in
      (List.find Tile.symmetries ~f:(fun g ->
           let oriented = g unoriented in
           Map.mem unduped_edges (edge_to_int (Tile.top oriented))
           && Map.mem unduped_edges (edge_to_int (Tile.left oriented)))
      |> Option.value_exn)
        unoriented
    in
    (* Fills a single row *)
    let fill ~(dir : [ `Under | `Right ]) (first_tile, first_tile_id) =
      let rec fill' row_rev =
        let far_edge, _near_edge =
          match dir with
          | `Under -> Tile.bottom, Tile.top
          | `Right -> Tile.right, Tile.left
        in
        let furthest_tile, furthest_tile_id = List.hd_exn row_rev in
        let furthest_edge = far_edge furthest_tile |> edge_to_int in
        match
          Map.find_exn tiles_by_edge furthest_edge
          |> List.find ~f:(( <> ) furthest_tile_id)
        with
        | None -> row_rev
        | Some next_tile_id ->
          let next_tile =
            match Tile.align furthest_tile (Map.find_exn tiles next_tile_id) with
            (* we could check that [next_tile_fits_in_dir = dir], but w/e *)
            | Some (next_tile, _next_tile_fits_in_dir) -> next_tile
            | _ -> failwith "oh no"
          in
          fill' ((next_tile, next_tile_id) :: row_rev)
      in
      List.rev (fill' [ first_tile, first_tile_id ])
    in
    let left_side = fill ~dir:`Under (top_left_tile, top_left_id) in
    let grid : Tile.t =
      List.map left_side ~f:(fill ~dir:`Right)
      |> List.map ~f:(List.map ~f:(fun (tile, _tile_id) -> Tile.trim_border tile))
      |> List.map ~f:Tile.concat_left_to_right
      |> List.concat
    in
    let num_rows, num_cols = List.length grid, List.length (List.hd_exn grid) in
    let grid = Tile.to_set grid in
    let sea_monsters : Pair.Set.t list =
      List.map Tile.symmetries ~f:(fun g -> g sea_monster) |> List.map ~f:Tile.to_set
    in
    let (in_monsters : Pair.Set.t) =
      List.init num_rows ~f:(fun i ->
          List.init num_cols ~f:(fun j ->
              List.map sea_monsters ~f:(Set.map (module Pair) ~f:(Pair.( + ) (i, j)))
              |> List.filter ~f:(fun sea_monster -> Set.is_subset ~of_:grid sea_monster)))
      |> List.concat
      |> List.concat
      |> List.concat_map ~f:Set.to_list
      |> Pair.Set.of_list
    in
    print_int (Set.diff grid in_monsters |> Set.length)
;;

let%expect_test _ =
  solve A Test_cases.problem20;
  let%bind () = [%expect {| 20899048083289 |}] in
  solve B Test_cases.problem20;
  let%bind () = [%expect {| 273 |}] in
  return ()
;;
