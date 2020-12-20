open! Core
open! Async
open Common
include Solution

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
           ; (module Problem21 : S)
           ; (module Problem22 : S)
           ; (module Problem23 : S)
           ; (module Problem24 : S)
           ; (module Problem25 : S)
           ]
           (problem - 1)
       in
       Reader.file_lines input_file >>| M.solve subpart)
;;
