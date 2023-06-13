(**  [Length of a List]
Find the number of elements of a list.
*)

let length lst =
  let rec aux acc = function
    | [] -> acc
    | _::xs -> aux (1+acc) xs
  in aux 0 lst
   
  

