open Core
open Common

module type S = sig
  val solve : Subpart.t -> string List.t -> unit
end

module Not_implemented : S = struct
  let solve subpart _file_contents =
    match (subpart : Subpart.t) with
    | A -> failwith "not implemented"
    | B -> failwith "not implemented"
  ;;
end
