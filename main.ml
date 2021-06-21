
open Type
open Color

open Axiom
open Symbol

open Display_console
open Output
open Printer

open Preprocessing

let lp_pkg = "tests"
let prelude_path = ["Tests"] (* depuis lp_pkg *)
let prelude_name = "prelude"

let get_filename name =
  let tmp = String.lowercase_ascii name in
  match !output with
  | Dedukti -> tmp ^ ".dk"
  | LP      -> tmp ^ ".lp"

let () =
  Cmd_line.parse ();
  let lexbuf = Lexing.from_channel (!Cmd_line.input) in
  let file = Kparser.file Klexer.token lexbuf in

  let module_to_file : kmodule -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    let len = List.length in

    let cd = reset_count_data 0 in

    let name, kimport_l, kcommand_l, _ = m in
    cd.k_import := len (kimport_l) ;
    let filename = get_filename name in
    print_header_file filename;

    print_nb_total_commands (len kcommand_l);

    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in

    List.iter (pp_import ff cd [lp_pkg]) (List.rev kimport_l);
    pp_import ff  cd (lp_pkg::prelude_path) (prelude_name, []);

    if !Cmd_line.old then Preprocessing.old ff m cd
    else
      begin
        let printing = match !output with
          | LP      -> Printer.pp_command ff cd
          | Dedukti -> Printer.pp_command ff cd
        in
        match !format with
        | Kore    -> List.iter printing kcommand_l
        | K       -> ()
        | Dedukti -> ()
      end;
    print_count_data cd;

    print_separator ();
    Format.pp_print_flush ff ();
    close_out f
  in
  print_header_kamelo ();
  List.iter module_to_file (snd file);
  print_footer_kamelo ();
  flush stdout;;
