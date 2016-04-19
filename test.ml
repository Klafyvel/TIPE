type element = {mutable visite:bool; suivants:int list};;

let elements = [|
  {visite=false; suivants=[1;3;4]};
  {visite=false; suivants=[0;2]};
  {visite=false; suivants=[1;4]};
  {visite=false; suivants=[0;4]};
  {visite=false; suivants=[3]}
|];;

let print_intlist l =
  print_string "[";
  let rec loop l = match l with
  | [] -> print_string "]"
  | hd::tl -> print_int hd;print_string ";";loop tl
  in
  loop l
;;

let print_bool b = if b then print_string "true" else print_string "false";;

let affiche_elem e =
  print_string "<Element n=";
  print_int e;
  print_string " suivants=";
  print_intlist elements.(e).suivants;
  print_string " visite=";
  print_bool elements.(e).visite;
  print_string ">"
;;

let trouve_suivant e =
  let rec loop l r = match l with
  | [] -> r
  | hd::tl when not( elements.(hd).visite) -> elements.(hd).visite<-true;loop tl (hd::r)
  | hd::tl -> loop tl r
  in
  loop elements.(e).suivants []
;;

type treeElement = {prev:treeElement option; value:int};;

let treeElement_of_list l origin =
  let rec loop l r = match l with
  | [] -> r
  | hd::tl -> loop tl ({prev=origin; value=hd}::r)
  in
  loop l []
;;

let check_generation l target =
  let rec loop l = match l with
  | [] -> false
  | hd::tl when hd.value = target -> true
  | hf::tl -> loop tl
  in
  loop l
;;

let next_generation g =
  let rec loop l r = match l with
  | [] -> r
  | hd::tl -> loop tl ((treeElement_of_list (trouve_suivant hd.value) (Some hd))@r)
  in
  loop g []
;;

let plusCourtChemin origin target=
  let rec loop g = if check_generation g target then g else
    let l = next_generation g in
    loop l
  in
  let choisi l =
    let rec loop l r = match l with
    | [] -> r
    | hd::tl when hd.value=target -> loop tl (hd::r)
    | hd::tl -> loop tl r
    in
    loop l []
  in
  let depile e =
    let rec loop e r = match e with
    | None -> r
    | Some x -> loop x.prev (x.value::r)
    in
    loop e.prev []
  in
  let depileTous l =
    let rec loop l r = match l with
    | [] -> r
    | hd::tl -> loop tl ((depile hd)::r)
    in
    loop l []
  in
  depileTous (choisi (loop [{prev=None;value=origin}]))
;;

let print_plusCourChemin l =
  let rec loop l = match l with
  | [] -> print_newline()
  | hd::tl -> print_intlist hd; loop tl
  in
  loop l
;;
print_plusCourChemin (plusCourtChemin 3 1);;
