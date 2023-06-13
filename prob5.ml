(** Reverse a list.*)

let rev lst =
  let rec loop acc  = function
    | [] -> acc
    | x::xs -> loop (x::acc) xs
  in loop [] lst
