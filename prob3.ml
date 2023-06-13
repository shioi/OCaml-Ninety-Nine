(**  [N'th Element of a List]
Find the N'th element of a list.
*)

let rec nth xs n =
  match xs, n with
  | [], _ -> None
  | x::xs, 0 -> Some x
  | x::xs, n -> nth xs (n-1)

