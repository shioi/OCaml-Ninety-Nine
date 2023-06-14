(** Drop every nth element *)

let drop lst n =
  let rec loop acc i = function
    | [] -> List.rev acc
    | x::xs -> if i = 1 then loop acc n xs
      else loop (x::acc) (i-1) xs
  in loop [] n lst
    

