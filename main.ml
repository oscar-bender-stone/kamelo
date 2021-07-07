open Type
open Display_console
open Output

let () =
  (* STEP A: Parse the command-line *)
  Cmd_line.parse ();
  (* STEP B: Parse the .kore file   *)
  let lexbuf = Lexing.from_channel (!Cmd_line.input) in
  let file = Kparser.file Klexer.token lexbuf in
  (* STEP C: Generate a file for each Kore module *)
  let module_to_file : kmodule -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    let name, kimport_l, kommand_l, _ = m in
    Dependency_graph.link := Dependency_graph.Link.empty ; (* @TODO arg *)
    (* STEP 0: Reset count data *)
    let cd = Count_data.reset_count_data 0 in
    (* STEP 1: Create the new file *)
    let filename = get_filename name in
    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in
    (* STEP 2: Import management *)
    Import.with_prelude ff kimport_l cd;
    (* STEP 3: Main translation *)
    if !Cmd_line.old then Preprocessing.old ff m cd
    else
      begin
        let printing = match !output with
          | LP      -> Printer.pp_kommand ff cd
          | Dedukti -> Printer.pp_kommand ff cd (* @TODO *)
          | Kore    -> Printer.pp_kore_kommand ff cd
        in
        match !mimic with
        | Kore    -> Printer.pp_kommand_ter ff cd kommand_l
        | K       -> List.iter printing kommand_l
        | Dedukti ->
           let g =
             Dependency_graph.create_dependence_graph cd kommand_l
           in
           let tmp node =
             try
               Some (Dependency_graph.Link.find node !Dependency_graph.link)
             with Not_found -> Format.printf (Color.yel "WARNING: %s need to be defined in prelude.lp\n") node ; None
           in
           let f node = match tmp node with | Some x -> printing x | None -> () in
           Dependency_graph.T.iter f g
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
