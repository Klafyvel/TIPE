open Core.Std
let json_of_graph_element n =
  `Assoc [
    ("degree",`Int (Graph.degree n));
    ("id", `Int n.id);
    ("link", `List (List.map ~f:(fun x -> `Int x) n.link))
  ]
;;
let save_graph ?filename:(filename="graph.json") g =
  	let file = Out_channel.create filename in
  	`List (Array.map ~f:(fun x -> json_of_graph_element x) g |> Array.to_list) 
  		|> Yojson.Basic.pretty_to_channel file;
  	Out_channel.close file
;;

let graph_element_of_json json =
  let open Yojson.Basic.Util in
  let open Graph in
  let id = json |> member "id" |> to_int in
  let link = json |> member "link" |> to_list |> List.map ~f:(fun x -> x |> to_int) in
  let degree = json |> member "degree" |> to_int in
  {id=id;link=link;degree=ref degree}
;;


let load_graph ?filename:(filename="graph.json") =
  let json = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  json |> to_list |> List.map ~f:graph_element_of_json |> Array.of_list
;;

let () =
let g = Graph.wattsStrogatz 20 5 0.7 in
save_graph g;;