(* Slice *)

let rec slice xs i k =
  match xs with
  | [] -> []
  | x::xs' -> begin
      match i, k with
      | 0, 1 -> []
      | 0, n -> x:: slice xs' 0 (n-1)
      | n, _ -> slice xs' (n-1) k
    end

(* |--------------------------------------------------------------------------|
   |****************************FOLD IMPLEMENTATION***************************|  
   |--------------------------------------------------------------------------|
*)

let rec fold_till f acc n = function
  | [] -> (acc,[])
  | x::xs' -> if n=0 then (acc,x::xs')
    else fold_till f (f x acc) (n-1) xs'

let slices xs i k =
  let _, x = fold_till (fun x y -> x :: y) [] i xs in
  let y, _  = fold_till (fun x y -> x :: y) [] (k-1) x in
  List.rev y
  
      


