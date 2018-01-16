(*
[analyse_file_1 file] ouvre le fichier [file] et retourne la liste des transitions et les points de dÃ©part et d'arrivÃ©e dÃ©crits dans ce fichier

LÃ¨ve l'exception :
 - Sys_error msg si le fichier n'est pas accessible
 - Scanf.Scan_failure msg si le fichier n'est pas au bon format
*)

val analyse_file_1 : string -> (string * string * int) list * (string * string)

val output_sol_1 : int -> string list -> unit


(*
[analyse_file_2 file] ouvre le fichier [file] et retourne la liste des transitions et la liste des chemins Ã  parcourir

LÃ¨ve l'exception :
 - Sys_error msg si le fichier n'est pas accessible
 - Scanf.Scan_failure msg si le fichier n'est pas au bon format
*)

val analyse_file_2 : string -> (string * string * int) list * string list list


(*
[output_sol_2 sol] affiche les solutions pour la phase 2.
[sol] est une liste de solutions[sol_1;sol_2;...sol_n].
Chaque [sol_i] est de la forme (path,times) oÃ¹ path est le chemin demandÃ© et times est la liste des temps de dÃ©part des diffÃ©rents module*)
val output_sol_2 :  (string list*int list) list -> unit

(*
[analyse_file_3 file] ouvre le fichier [file] et retourne la liste des transitions et la liste des points de dÃ©part et d'arrivÃ©e de chacun.

LÃ¨ve l'exception :
 - Sys_error msg si le fichier n'est pas accessible
 - Scanf.Scan_failure msg si le fichier n'est pas au bon format
*)

val analyse_file_3 : string -> (string * string * int) list * (string *string) list
