(**  [Eliminate Duplicates]
     Eliminate consecutive duplicates of list elements. *)

let rec compress = function
  | x::(y::_ as rest) -> if x = y then compress rest
    else x :: compress (rest)
  |others -> others
