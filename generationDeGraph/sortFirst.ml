open Core.Std;;

let sortFirst n l =
  let rec sep p l left right len_l = match l with
  | [] -> left, right, len_l
  | (a,b)::tl when a > p -> sep p tl ((a,b)::left) right (len_l + 1)
  | (a,b)::tl -> sep p tl left ((a,b)::right) len_l
  in
  let rec loop n l = match l with 
  | [] -> []
  | (a,b)::tl -> 
    let left, right, len_l = sep a tl [] [] 0 in
    if len_l >= n then (loop n left)
    else
    (loop n left)@[(a,b)]@(loop (n-len_l-1) right)
  in
  List.map (loop n l) ~f:(fun (a,b) -> b)
;;
