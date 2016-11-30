(*
let live g first =
  let infected = Array.make 10000 (false,0) in
  let rec loop l i = match l with
  | [] -> ()
  | hd::tl when fst infected.(hd) -> loop tl i
  | hd::tl -> infected.(hd) <- (true,i);loop tl i;loop g.(hd).link (i+1)
  in
  loop first 0;
  infected
;;
*)
let () =
let g = Graph.wattsStrogatzMatrix 100 10 0.6 in
print_endline "Génération terminée";
let btc = Graph.betweenness g in
Array.iteri (fun i a -> print_string "i:"; print_int i;print_string " btc:";print_float a;print_newline ()) btc (*
Array.iter (fun (_,x)->print_int x;print_newline()) (live())*)
;; 