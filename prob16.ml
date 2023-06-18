(*  Split a List Into Two Parts; The Length of the First Part Is Given
Beginner difficulty

    Split a list into two parts; the length of the first part is given. *)

let rec aux acc n = function
  | [] -> List.rev acc, []
  | x::xs -> if n = 1 then List.rev (x::acc), xs
    else aux (x::acc) (n-1) xs


let split xs n = aux [] n xs
  
