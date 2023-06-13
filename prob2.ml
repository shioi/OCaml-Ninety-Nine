(**  [Last Two Elements of a List]
Find the last but one (last and penultimate) elements of a list.
*)

let rec last_two = function
  | _::[] | [] -> None
  | x::y::[] -> Some (x,y)
  | _::xs -> last_two xs
