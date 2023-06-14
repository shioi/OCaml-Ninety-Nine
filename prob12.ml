(** Decode a Run-Length Encoded List  *)

type 'a rle =
  | One of 'a
  | Many of int * 'a


let rec decode = function
  | [] -> []
  | One x::xs -> x :: decode xs
  | Many(x,y) :: xs -> if x > 1 then y :: decode (Many((x-1),y)::xs)
    else y:: decode xs
