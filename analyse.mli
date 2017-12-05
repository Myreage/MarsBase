(* 
[analyse_file_1 file] ouvre le fichier [file] et retourne la liste des transitions et les points de départ et d'arrivée décrits dans ce fichier

Lève l'exception : 
 - Sys_error msg si le fichier n'est pas accessible
 - Scanf.Scan_failure msg si le fichier n'est pas au bon format
*) 

val analyse_file_1 : string -> (string * string * int) list * (string * string)

val output_sol_1 : int -> string list -> unit
                                                                
