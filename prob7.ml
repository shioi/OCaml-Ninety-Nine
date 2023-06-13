(** Flatten a nested list structure.*)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let rev lst =
  let rec loop acc  = function
    | [] -> acc
    | x::xs -> loop (x::acc) xs
  in loop [] lst


let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One x::xs -> aux (x::acc) xs
    | Many x::xs -> aux (aux acc x) xs
  in aux [] lst |> rev



