open Common.Error
open Mecanism.Count_data

(** [print_info (cran, nb, one, several)] prints only one result of
    the review, where:
      - [cran] is the number of indentations before printing the result,
      - [nb]   is the number of the specific command,
      - [one]     is the printing message if [nb] = 1,
      - [several] is the printing message if [nb] >= 2. *)
let print_info : int * int * string * string -> unit =
  fun (cran, nb, one, several) ->
  let text =  if nb >= 2 then several else one in
  if nb <= 0 then ()
  else
    let nb = string_of_int nb in
    match cran with
    | 0 -> msg_2       _STDOUT "  %s %s" nb text
    | 1 -> cyan_msg_2  _STDOUT "    * %s %s" nb text
    | 2 -> green_msg_2 _STDOUT "       - %s %s" nb text
    | _ -> raise (KaMeLoError (InternalError, "Display_console", "print_info", ""))

let print_count_data : count_data -> unit = fun cd ->
  red_msg _STDOUT "Before translating..." ;
  List.iter print_info (extract_info_before cd) ;
  red_msg _STDOUT "...after translating:" ;
  List.iter print_info (extract_info_after cd)

let print_header_kamelo : unit -> unit = fun () ->
  green_msg _STDOUT
    "-------------------- Welcome to KaMeLo ---------------------"
let print_header_file filename =
  blue_msg_1 _STDOUT "--- Translation of %s" filename

let print_nb_total_commands nb =
  red_msg_1 _STDOUT "There are %i commands." nb

let separator =
  "------------------------------------------------------------"
let print_footer_file : unit -> unit =
  fun () -> print _STDOUT "%s\n" separator

let print_footer_kamelo : unit -> unit =
  fun () -> green_msg_1 _STDOUT "%s" separator

let print_module_message : string -> int -> count_data -> unit =
  fun filename nb cd ->
  print_header_file filename;
  print_nb_total_commands nb;
  print_count_data cd;
  print_footer_file ()
