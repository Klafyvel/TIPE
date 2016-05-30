type tValues =
  | Int of int
  | Infinite
;;

type graphe = bool array array ;;
let create n = Array.make_matrix n n Infinite ;;

let infTValues (a:tValues) (b:tValues) = match a,b with
  | Int(_),Int(_) -> a<b
  | Int(_),Infinite -> true
  | _ -> false
;;

let addTValues a b = match a,b with
  | Int(i),Int(j) -> Int(i+j)
  | _ -> Infinite
;;

let print_tValue t = match t with
  | Int(v) -> print_int v
  | Infinite -> print_string "∞"
;;
let rec print_Tlist l = match l with
  | [] -> print_newline()
  | hd::tl -> print_tValue hd; print_Tlist tl
;;

let rec print_list l = match l with
  | [] -> print_newline()
  | hd::tl -> print_int hd;print_string ","; print_list tl
;;
let trouveTkMinimal e t =
  let rec loop min value e = match e with
  | [] -> min
  | hd::tl when t.(hd)=Int(0) -> loop min value tl
  | hd::tl when infTValues t.(hd) value ->loop hd t.(hd) tl
  | hd::tl ->loop min value tl
  in
  loop 0 Infinite e
;;

let print_t t =
  let length = Array.length t in
  for i = 0 to (length-1) do
    print_tValue t.(i)
  done;
;;

let rec uneEtape e f t graphe =
  print_string "Etape";print_newline();
  print_string "t="; print_t t;print_newline();
  let length = Array.length t in
  let rec update e f l k =(*
    print_string "Sous étape.";print_newline();
    print_string "e=";print_list e;
    print_string "f=";print_list f;
    print_string "l=";print_int l;
    for i=0 to 10000 do
      print_string "\r";
    done;
    print_newline();*)
    if l >= length then e else
    let sum = addTValues t.(k) graphe.(k).(l) in
    if (infTValues graphe.(k).(l) Infinite) && not (List.mem l f) && not (List.mem l e) then update (l::e) f l k
    else if infTValues sum t.(l) then
      begin
        t.(l) <- sum;
        (update e f (l+1) k)
      end
    else update e f (l+1) k
  in
  let k = trouveTkMinimal e t in
  print_string "k=";print_int k;print_newline();
  match e with
  | [] -> f
  | hd::tl -> uneEtape (update (List.filter (fun x -> x<>k) e) (k::f) 0 k) (k::f) t graphe
;;


let plusCourtChemin a b graphe s =
  let t = Array.init s (fun x -> if x=a then Int(0) else Infinite) in
  let f = uneEtape [a] [] t graphe in
  print_t t
;;


let graphe = [|
  [| Infinite; Int(1); Infinite; Infinite; Infinite; Infinite |];
  [| Infinite; Infinite; Infinite; Infinite; Infinite; Int(1) |];
  [| Infinite; Infinite; Infinite; Infinite; Int(1); Infinite |];
  [| Int(1); Infinite; Infinite; Infinite; Infinite; Int(1) |];
  [| Infinite; Int(1) ; Infinite; Infinite; Infinite; Int(1) |];
  [| Int(1); Infinite; Int(1); Int(1); Infinite; Infinite |]
|];;

plusCourtChemin 0 2 graphe 6;;


(*print_string (string_of_bool (infTValues (Int 0) Infinite));print_newline();;*)
