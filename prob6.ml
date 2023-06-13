(** List palindrome function *)

let rev lst =
  let rec loop acc  = function
    | [] -> acc
    | x::xs -> loop (x::acc) xs
  in loop [] lst

let is_palindrome lst = rev lst = lst
