open Core.Std

open Random;;

Random.self_init();;

type team =
  | Black
  | White

type graphNode = { team : team; id : int; link : int list }

type graph = { entry : int; nodes : graphNode array; size : int}

(*
let gen_graph s nMax =
  let grid = Array.make_matrix ~dimx:s ~dimy:s false in
  let nbLink = Array.init s ~f:(fun x -> ((Random.int (nMax-1))+1,0)) in
  let addLink i =
    let rec loop j = if j >= s then () else
    match nbLink.(j) with
    | m,n when n >= m || grid.(i).(j) -> loop (j+1)
    | m,n -> grid.(i).(j) <- true;grid.(j).(i) <- true;
      let mi,ni = nbLink.(i) in nbLink.(i) <- (mi,ni+1);
      nbLink.(j) <- (m,n+1)
    in
    loop (0)
  in
  let rec loop i = if i < 0 then () else
    let max,n = nbLink.(i) in
    if n >= max then loop (i-1) else begin
    for j = 1 to max-n do
      addLink i
    done;
    loop (i-1)
    end;
  in
  loop (s-1);
  let getLinkList i =
    let rec loop j res = if j<0 then res else
      if grid.(j).(i) then loop (j-1) (j::res) else loop (j-1) res
    in
    loop (s-1) []
  in
  {
    entry = 0;
    nodes = Array.init s ~f:(fun x -> let link = getLinkList x in
      if (x mod 2)=0 then {team=Black;id=x;link=link} else {team=White;id=x;link=link});
    size = s
    }
;;
*)

let gen_graph s nMax =
  let rec loop l c i = if (List.length l) + (List.length c) >= s then l @ c else
    match c with
    | [] -> loop l [{team=White;id=i+1;link=[]}]
    | hd::tl ->


let live graph max_gen team =
  (* Test du tableau des noeuds visitées *)
  let array_full a test =
    let r = ref true in
    for i = 0 to Array.length a do
      r := !r && (a.(i) = test)
    done;
    !r
  in
  (* Noeuds visités *)
  let nodesStates = Array.create ~len:graph.size false in
  (* Sélectionne les noeuds enfants valides (non encore informés) d'un noeud. *)
  let rec pick_next index res = match index with
  | [] -> res
  | hd::tl when nodesStates.(hd)=false -> nodesStates.(hd) <- true;pick_next tl (hd::res)
  | _::tl -> pick_next tl res
  in
  (* Génère la génération suivant une génération. *)
  let rec next_gen alives res = match alives with
  | [] -> res
  | hd::tl when graph.nodes.(hd).team=team-> next_gen tl ((pick_next graph.nodes.(hd).link [])@res)
  | _::tl -> next_gen tl res
  in
  (* Fait vivre toute les génération jusqu'à ce que tout les points soient
   * informé ou que le nombre maximal d'itérations soit dépassé.
   *)
  let rec every_gen alives n = if (n >= max_gen) || (array_full nodesStates true) then n else
    every_gen (next_gen alives []) (n+1)
  in
  (every_gen [graph.entry] 0, nodesStates)
;;

let string_of_team t = match t with
  | White -> "White"
  | Black -> "Black"
;;
let team_of_string s = match s with
  | "White" -> White
  | "Black" -> Black
  | _ -> failwith "Unhandled team."
;;

let json_of_node n =
  `Assoc [
    ("team",`String (string_of_team n.team));
    ("id", `Int n.id);
    ("link", `List (List.map ~f:(fun x -> `Int x) n.link))
  ]
;;
let saveGraph ?filename:(filename="graph.json") g =
  let file = Out_channel.create filename in
  `Assoc [
    ("entry", `Int g.entry);
    ("nodes", `List (Array.map ~f:(fun x -> json_of_node x) g.nodes |> Array.to_list));
    ("size", `Int g.size)
  ] |> Yojson.Basic.pretty_to_channel file;
  Out_channel.close file
;;

let node_of_json json =
  let open Yojson.Basic.Util in
  let team = json |> member "team" |> to_string |> team_of_string in
  let id = json |> member "id" |> to_int in
  let link = json |> member "link" |> to_list |> List.map ~f:(fun x -> x |> to_int) in
  {team=team;id=id;link=link}
;;


let loadGraph ?filename:(filename="graph.json") =
  let json = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  let entry = json |> member "entry" |> to_int in
  let size = json |> member "size" |> to_int in
  let nodes = json |> member "nodes" |> to_list |> List.map ~f:node_of_json |> Array.of_list in
  {entry=entry;nodes=nodes;size=size}
;;


let isConnex g =
  let states = Array.create g.size false in
  let rec loop s = if List.is_empty s then () else
    let q = Queue.create () in
    List.iter ~f:(fun x -> if not states.(x) then begin
      states.(x) <- true;
      List.iter ~f:(fun x -> Queue.enqueue q x) g.nodes.(x).link
    end;
    ) s;
    loop (Queue.to_list q)
  in
  loop [g.entry];
  for i = 0 to (g.size - 1) do
    print_string (string_of_bool states.(i))
  done;
  print_newline ();
  not (Array.fold ~init:false ~f:(||) states)
;;


let () =
  let g = loadGraph ~filename:"graph.json" in
  print_string (string_of_bool (isConnex g));
  saveGraph ~filename:"graph2.json" g
;;
