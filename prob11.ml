(**  [Modified Run-Length Encoding]

     Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list.
     Only elements with duplicates artransferred as (N E) lists.
*)


type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let rec loop acc xs =
    match acc, xs with
    | _, [] -> List.rev acc
    | [], x::xs' -> loop [One x] xs'
    | (One a::ac as t), x::xs' -> if a = x then loop (Many (2,x)::ac) xs'
      else loop (One x::t) xs'
    | (Many (a,b)::ac as t), x::xs' -> if b = x then loop (Many (a+1,b)::ac) xs'
      else loop (One x::t) xs'

  in loop [] lst
