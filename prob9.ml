(** 
   [Pack Consecutive Duplicates]
   Pack consecutive duplicates of list elements into sublists.
*)

let pack lst =
  let rec loop acc lst =
    match acc, lst with
    | ac, [] -> ac
    | [], x -> loop [[]] x
    | a::c, [x] -> (x::a)::c
    | a::c,x::(y::_ as t) -> if x = y then loop ((x::a)::c) t
      else loop ([]::(x::a)::c) t
  in loop [] lst |> List.rev
  
