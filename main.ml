open Common.Type
open Core.Display_console
open LP_interface.Output

let () =
  (* STEP A: Parse the command-line *)
  Core.Cmd_line.parse ();
  (* STEP B: Parse the .kore file   *)
  let lexbuf = Lexing.from_channel (!Core.Cmd_line.input) in
  let file = Parsing.Kparser.file Parsing.Klexer.token lexbuf in
  (* STEP C: Generate a file for each Kore module *)
  let module_to_file : kmodule -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    let name, kimport_l, kommand_l, _ = m in
    Core.Dependency_graph.data_syntax := LP_interface.LP_p_term.StrMap.empty ; (* @TODO arg *)
    (* STEP 0: Reset count data *)
    let cd = Common.Count_data.reset_count_data 0 in
    (* STEP 1: Create the new file *)
    let filename = get_filename name in
    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in
    (* STEP 2: Import management *)
    let printing = match !output with
          | LP      -> LP_interface.LP_printer.pp_command
          | Dedukti -> fun _ _ -> () (* @TODO *)
          | Kore    -> fun _ _ -> () (* Printer.pp_kore_kommand ff cd *)
    in
    Translation.Import.with_prelude ff printing kimport_l cd;
    (* STEP 3: Main translation *)
    if !Core.Cmd_line.old then Core.Preprocessing.old ff m cd
    else
      begin
        match !mimic with
        | Kore    -> (match !output with
                      | LP | Dedukti -> Translation.Printer.pp_kommand_ter ff cd printing kommand_l
                      | Kore -> Translation.Printer.pp_kore_kommand ff cd kommand_l)
        | K       -> Translation.Printer.pp_kommand_bis ff cd printing kommand_l
        | Dedukti ->
           let g =
             Core.Dependency_graph.create_dependence_graph cd kommand_l
           in
           let tmp node =
             try
               Some (LP_interface.LP_p_term.StrMap.find node !Core.Dependency_graph.data_syntax)
             with Not_found -> (if not(LP_interface.LP_p_term.StrMap.mem node !Core.Dependency_graph.in_prelude)
                                then Format.printf (Common.Color.yel "WARNING: Need to be fixed: %s doesn't exist.\n") node ; None)
           in
           let f node = match tmp node with | Some x -> Translation.Printer.pp_kommand ff cd printing x | None -> () in
           Core.Dependency_graph.T.iter f g
      end;
    (* STEP 4: Printing count data *)
    print_module_message filename (List.length kommand_l) cd;
    Format.pp_print_flush ff ();
    close_out f
  in
  (* STEP D: Iteration on .kore file *)
  print_header_kamelo ();
  List.iter module_to_file (snd file);
  print_footer_kamelo ();
  flush stdout;;
