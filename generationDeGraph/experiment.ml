(* Structure de la table experiments:
CREATE TABLE "experiments" ("name" TEXT,"last_id" INT,"infos" TEXT DEFAULT (null) )
*)


let load_db () =
  Sqlite3.db_open "experiments.sqlite"
;;

let close_db db =
  Sqlite3.db_close db
;;

let cleaner target row = 
  let l = Array.length row in
  for i = 0 to l-1 do
    target := Some(row.(i))
  done;
;;

let get_exp_last_id db name =
  let last_id = ref None in
  let get_last_id () =
    ignore (Sqlite3.exec_not_null_no_headers db ~cb:(cleaner last_id) ("SELECT last_id FROM experiments WHERE name=\""^name^"\";"))
  in
  get_last_id ();
  ! last_id
;;

let get_experiment db name =
  let last_id = get_exp_last_id db name in
  let last_result = ref None in
  let create_table () =
    ignore (Sqlite3.exec db ("CREATE TABLE "^name^" (id INT, value TEXT);"));
    ignore (Sqlite3.exec db ("INSERT INTO experiments VALUES (\""^name^"\", 0, \"\");"))
  in
  let get_last_result id =
    ignore (Sqlite3.exec_not_null_no_headers db ~cb:(cleaner last_result) ("SELECT value FROM "^name^" WHERE id=\""^id^"\";"))
  in
  let get_last_step () = match last_id with
  | None -> create_table (); !last_result
  | Some s -> get_last_result s; !last_result
  in
  get_last_step ()
;;

let add_step_id db exp id str =
  ignore (Sqlite3.exec db ("INSERT INTO "^exp^" VALUES ("^id^",\""^str^"\");"))
;;

let change_last_id db exp id =
  ignore (Sqlite3.exec db ("UPDATE experiments SET last_id = "^(string_of_int id)^" WHERE name=\""^exp^"\";"))
;;
let add_step db exp str = let id =
  match get_exp_last_id db exp with
  | None -> 0
  | Some(s) ->  (1 + (int_of_string s))
  in
  add_step_id db exp (string_of_int id) str;
  change_last_id db exp id;
;;
