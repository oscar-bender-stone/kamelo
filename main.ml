open Common.Type
open Terminal.Display_console
open LP_interface.Output

let () =
  (* STEP A: Parse the command-line *)
  Terminal.Cmd_line.parse ();
  (* STEP B: Parse the .kore file   *)
  let lexbuf = Lexing.from_channel (!Terminal.Cmd_line.input) in
  let file = Parsing.Kparser.file Parsing.Klexer.token lexbuf in
  match file with
  | F_exec(exec) ->
     (* STEP C: Create the new file *)
     let name = !filename_exec in
     let filename = create_filename name in
     let f  = open_out filename in
     let ff = Format.formatter_of_out_channel f in
     (* STEP D: Translate the executable *)
     let p_exec = Translation.Axiom.curry_ident exec in
     LP_interface.LP_printer.pp_command ff
       (LP_interface.LP_p_term.create_compute_command p_exec);
     (* STEP E: Close the new file *)
     Format.pp_print_flush ff ();
     close_out f ;
     flush stdout
  | F_sem (_, file) ->
     (* STEP 1: Create the new file *)
     let semantics_module_name =
       match List.map (fun (name,_,_,_) -> name) file with
       | ["BASIC-K";"KSEQ";"INJ";"K";x] -> x
       | _ -> failwith "WARNING: The prelude isn't the one expected."
     in
     let filename = create_filename semantics_module_name in
     let f  = open_out filename in
     let ff = Format.formatter_of_out_channel f in

     (* STEP : Import the prelude. *)
     (* let cd = Common.Count_data.reset_count_data 0 in *)
     (* Nécessaire si on veut séparer prelude.lp du reste
   let lp_pkg = "root_KaMeLo" in
  let prelude_name = "prelude" in *)
     (* STEP 2: Import management *)
     let printing = match !output with
       | LP      -> LP_interface.LP_printer.pp_command
       | Dedukti -> fun _ _ -> () (* @TODO *)
       | Kore    -> fun _ _ -> () (* Printer.pp_kore_kommand ff cd *)
     in
     (* cd.k_import := 1;
  pp_import ff cd printing (lp_pkg) (prelude_name, []);
      *)
     (* STEP : Create prelude *)
     (* 1. Translate BASIC-K *)
     (*    let name,_,kommand_l,_ = head (snd file) in
    (* 2. Add the injection _INJD *)

    (* 3. Generate the prelude *)
    create_prelude ff cd printing ; *)

     (* STEP : Translation of the K prelude. *)

     (* STEP : Translation of the semantics module. *)

     (* STEP C: Generate a file for each Kore module *)
     let module_to_file : kmodule -> unit = fun m ->
       (* let name, import_l, command_l, attribut_l = m in *)
       let name, _, kommand_l, _ = m in
       (* Mecanism.Dependency_graph.data_syntax := LP_interface.LP_p_term.StrMap.empty ; @TODO arg *)
       (* STEP 0: Reset count data *)
       let cd = Common.Count_data.reset_count_data 0 in
       (* STEP 1: Create the new file *)
       (* let filename = get_filename name in
    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in *)

       (* Printing.Import.with_prelude ff printing kimport_l cd; *)
       (* STEP 3: Main translation *)
       if !Terminal.Cmd_line.old then Terminal.Preprocessing.old ff m cd
       else
         begin
           match !mimic with
           | Kore    ->
              (match !output with
               | LP | Dedukti ->
                  Printing.Printer.encoding ff cd printing kommand_l (*Printing.Printer.pp_kommand_ter ff cd printing kommand_l*)
               | Kore -> Printing.Printer.pp_kore_kommand ff cd kommand_l)
           | K       -> Printing.Printer.encoding ff cd printing kommand_l
           (* Printing.Printer.pp_kommand_bis ff cd printing kommand_l *)
           | Dedukti -> ()
         (*  let g =
             Mecanism.Dependency_graph.create_dependence_graph cd kommand_l
           in
           let tmp node =
             try
               Some (LP_interface.LP_p_term.StrMap.find node !Mecanism.Dependency_graph.data_syntax)
             with Not_found -> (if not(LP_interface.LP_p_term.StrMap.mem node !Mecanism.Dependency_graph.in_prelude)
                                then Format.printf (Common.Color.yel "WARNING: Need to be fixed: %s doesn't exist.\n") node ; None)
           in
           let f node = match tmp node with | Some x -> Printing.Printer.pp_kommand ff cd printing x | None -> () in
           Mecanism.Dependency_graph.T.iter f g *)
         end;
       (* STEP 4: Printing count data *)
       print_module_message name (List.length kommand_l) cd;
       Format.pp_print_flush ff ();
       (* close_out f *)
     in
     (* STEP D: Iteration on .kore file *)
     print_header_kamelo ();
     module_to_file (List.hd file) ;
     Printing.Prelude.create_prelude ff printing "prelude" ; (* Transformer en module pour ne plus à qu'à itérer ? *)
     List.iter module_to_file (List.tl file);
     print_footer_kamelo ();
     close_out f;
     flush stdout;;
