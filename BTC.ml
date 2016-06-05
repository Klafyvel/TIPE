open Stack;;
open Queue;;

type graphe = bool array array ;;
let create n = Array.make_matrix n n false ;;

let rec print_list l = match l with
  | [] -> print_newline()
    | hd::tl -> print_int hd;print_string ","; print_list tl
;;

let graphe = [|
  [| false; true; false; false; false; false|];
  [| false; false; false; false; false; true|];
  [| false; false; false; false; true; false|];
  [| true; false; false; false; false; true|];
  [| false; true; false; false; false; true|];
  [| true; false; true; true; false; false|]
|];;

let n = 6;;

let betweenness g n =
  let cB = Array.make n 0.0 in
  for s = 0 to n-1 do
    let stack = Stack.create() in
    let p = Array.make n [] in
    let sigma = Array.make n 0.0 in
    sigma.(s) <- 1.0;
    let d = Array.make n (-1) in
    d.(s) <- 0;
    let q = Queue.create() in
    Queue.push s q;
    while not (Queue.is_empty q) do
      let v = Queue.pop q in
      Stack.push v stack;
      for w = 0 to (n-1) do
        let iw = g.(v).(w) in
        if iw then
          if d.(w) < 0 then begin
            Queue.push w q;
            d.(w) <- d.(v) + 1;
          end;
          if d.(w) = (d.(v) + 1) then begin
            sigma.(w) <- sigma.(w) +. sigma.(v);
            p.(w) <- v::p.(w);
          end;
      done;
    done;
    let delta = Array.make n 0.0 in
    while not (Stack.is_empty stack) do
      let w = Stack.pop stack in
      List.iter (fun v -> delta.(v) <- delta.(v) +. sigma.(v) /. sigma.(w) *. (1.0 +. delta.(w))) p.(w);
      if w != s then begin cB.(w) <- cB.(w) +. delta.(w); end;
    done;
  done;
  cB
;;


betweenness graphe n;;
