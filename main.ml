open Common.Type
open Common.Xlib_OCaml
open Interface.Signature
open Terminal.Display_console

open Controller.Prelude (* TODO delete *)


let () =
  (* STEP A: Parse the command-line *)
  Terminal.Cmd_line.parse ();
  (* STEP B: Parse the .kore file   *)
  let lexbuf = Lexing.from_channel (!Terminal.Cmd_line.input) in
  let file = Parsing.Kparser.file Parsing.Klexer.token lexbuf in
  (* STEP C: Translate the semantic or the executable *)
  match file with
  | F_exec(exec) ->
     (* STEP 1: Create the new file *)
     let name = !Terminal.Cmd_line.filename_exec in
     let filename = Terminal.Cmd_line.create_filename name in (* TODO *)
     let f  = open_out filename in
     let ff = Format.formatter_of_out_channel f in
     (* STEP 2: Add the import of the semantic *)
     let cd = Mecanism.Count_data.reset_count_data 0 in
     let path = ["sem_root"] in
     let i = (!Terminal.Cmd_line.semantics_file, []) in
     let import_trans =
       Translating.Import.import_to_require_open path i
     in
     Mecanism.Count_data.incr_real_import cd ;
     LP.LP_printer.pp_command ff import_trans ;
     (* STEP 3: Translate the executable *)
     let p_exec, free_var_data = Translating.Executable.curry_exec_ident exec empty_sign in
     (* STEP 4: Add free variables *)
     let f_pp : string -> string list -> unit = fun key var_l ->
       let var_type =
         Interface.LP_p_term.create_appl
           Interface.K_prelude.p_INJD
           (Interface.LP_p_term.create_ident key) in
       let comm name =
         let _ = Controller.Prelude.pp_symbol_prelude ff cd
           (LP.LP_printer.pp_command) empty_sign
           (Interface.LP_p_term.create_symbol name var_type) in ()
       in
       List.iter (fun name -> comm name) var_l
     in
     StrMap.iter f_pp free_var_data ;
     (* STEP 5: Printing *)
     LP.LP_printer.pp_command ff
       (Interface.LP_p_term.create_compute_command p_exec);
     (* STEP 6: Close the new file *)
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
     let filename =
       Terminal.Cmd_line.create_filename semantics_module_name (* TODO *)
     in
     let f  = open_out filename in
     let ff = Format.formatter_of_out_channel f in

     (* STEP : Import the prelude. *)
     (* Nécessaire si on veut séparer prelude.lp du reste
        let lp_pkg = "root_KaMeLo" in
        let prelude_name = "prelude" in *)

     (* STEP 2: Import management *)
     let printing = match !Terminal.Cmd_line.output with
       | O_LP      -> LP.LP_printer.pp_command
       | O_Dedukti -> fun _ _ -> () (* @TODO *)
       | O_Kore    -> fun _ _ -> () (* Printer.pp_kore_kommand ff cd *)
     in
     (* STEP 3: Generate the prelude *)

     (* STEP : Translation of the K prelude. *)

     (* STEP : Translation of the semantics module. *)

     (* STEP C: Generate a file for each Kore module *)
     let module_to_file : signature -> kmodule -> signature =
       fun sign m ->
       (* let name, import_l, command_l, attribut_l = m in *)
       let name, _, kommand_l, _ = m in
       (* STEP 0: Cleaning the semantic files *)
       let kommand_l =
         if not(name = "BASIC-K" || name = "KSEQ" || name = "INJ" || name = "K") then
           Translating.Cleaning.cleaning kommand_l
         else kommand_l
       in
       Common.Error.print ff "\n// Translation of the module ";
       Format.pp_print_string ff name;
       Common.Error.print ff "\n";
       (* Mecanism.Dependency_graph.data_syntax :=
           LP_interface.LP_p_term.StrMap.empty ; @TODO arg *)
       (* STEP 0: Reset count data *)
       let cd = Mecanism.Count_data.reset_count_data 0 in

       (* STEP 3: Main translation *)
       let sign_bis =
         if !Terminal.Cmd_line.old then
           (Controller.Old.first_translation ff cd m ; sign)
         else
           begin
             let res, new_sign =
               Controller.With_Viry_encoding.main cd kommand_l sign
             in
             (match !Terminal.Cmd_line.mimic with
              | M_Kore    ->
                (match !Terminal.Cmd_line.output with
                 | O_LP | O_Dedukti ->
                    Printing.Meta_printer.prt_Viry ff cd printing res
               (*Printing.Printer.pp_kommand_ter ff cd printing kommand_l*)
                 | O_Kore ->
                    Printing.Kore_printer.pp_kore_kommand ff cd kommand_l)
             | M_K       ->
                Printing.Meta_printer.prt_Viry ff cd printing res
           (* Printing.Printer.pp_kommand_bis ff cd printing kommand_l *)
             | M_Dedukti -> ()) ; new_sign
         (*  let g =
             Mecanism.Dependency_graph.create_dependence_graph cd kommand_l
           in
           let tmp node =
             try
               Some (LP_interface.LP_p_term.StrMap.find node !Mecanism.Dependency_graph.data_syntax)
             with Not_found -> (if not(LP_interface.LP_p_term.StrMap.mem node !Mecanism.Dependency_graph.in_prelude)
                                then wrn_1 "WARNING: Need to be fixed: %s doesn't exist." node ; None)
           in
           let f node = match tmp node with | Some x -> Printing.Printer.pp_kommand ff cd printing x | None -> () in
           Mecanism.Dependency_graph.T.iter f g *)
           end
       in
       (* STEP 4: Printing count data *)
       print_module_message name (List.length kommand_l) cd;
       Format.pp_print_flush ff () ; sign_bis
     in
     (* STEP D: Iteration on .kore file *)
     print_header_kamelo ();
     let sign_g = module_to_file empty_sign (List.hd file) in
     print_comment ff "PRELUDE";
     let sign_res =
       Controller.Prelude.create_prelude ff printing sign_g "prelude"
     in
     (* Transformer en module pour ne plus avoir qu'à itérer ? *)
     let _ = List.fold_left module_to_file sign_res (List.tl file) in
     (*
     let rec update_signature l acc = match l with
       | []   -> acc
       | h::q -> update_signature q (module_to_file acc h)
     in
     let _ = update_signature (List.tl file) sign_res in *)
     print_footer_kamelo ();
     close_out f;
     flush stdout;;
