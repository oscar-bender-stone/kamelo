
open Color
open Count_data

let print_info : int option * int option * string * string -> unit = fun (i, j, one, several) ->
  let denomi = match j with
    | None -> "?"
    | Some x -> string_of_int x
  in
  let is_zero = (j = Some 0) in
  match i with
  | None -> if not(is_zero) then Format.printf (cya "  * %s %s\n") denomi several
  | Some i ->
     if is_zero && i = 0
     then ()
     else
       if i < 2
       then Format.printf "%i / %s %s translated.\n" i denomi one
       else Format.printf "%i / %s %s translated.\n" i denomi several

let print_count_data : count_data -> unit = fun cd -> List.iter print_info (extract_info cd)

let print_header_kamelo : unit -> unit = fun () ->
  Format.printf (gre "-------------------- Welcome to Kamelo ---------------------\n")
let print_header_file filename =
  Format.printf (blu "--- Translation of %s\n") filename

let print_nb_total_commands nb = Format.printf (red "There are %i commands\n") nb

let separator = "------------------------------------------------------------\n"
let print_footer_file : unit -> unit = fun () -> Format.printf "%s" separator

let print_footer_kamelo : unit -> unit = fun () -> Format.printf (gre "%s") separator

let print_module_message : string -> int -> count_data -> unit = fun filename nb cd ->
  print_header_file filename;
  print_nb_total_commands nb;
  print_count_data cd;
  print_footer_file ()
