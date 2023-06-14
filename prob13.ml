(** Run-Length Encoding *)

let encode lst =
  let rec loop acc xs =
    match acc, xs with
    | _, [] -> List.rev acc
    | [], x::xs' -> loop [(1,x)] xs'
    | ((a,b)::ac as t), x::xs' -> if b = x then loop ((a+1,b)::ac) xs'
      else loop ((1,x)::t) xs'
  in loop [] lst
