open Common.Type
open Common.Xlib_OCaml
open Interface.Signature
open Terminal.Display_console

open Controller.Prelude (* TODO delete *)


let () =
  (* STEP A: Parse the command-line *)
  Terminal.Cmd_line.parse ();
  (* STEP B: Parse the Kore file   *)
  let lexbuf = Lexing.from_channel (!Terminal.Cmd_line.input) in
  let file = Parsing.Kparser.file Parsing.Klexer.token lexbuf in
  (* STEP C: Translate the semantic or the executable *)
  match file with
  | F_exec(exec, result) ->
     (* STEP 1: Create the new file *)
     let name = !Terminal.Cmd_line.filename_exec in
     let filename = Terminal.Cmd_line.create_filename name in (* TODO *)
     let f  = open_out filename in
     let ff = Format.formatter_of_out_channel f in
     (* STEP 2: Print the import of the semantic *)
     let cd = Mecanism.Count_data.reset_count_data 0 in
     let path = ["sem_root"] in
     let i = (!Terminal.Cmd_line.semantics_file, []) in
     let import_trans =
       Translating.Import.import_to_require_open path i
     in
     Mecanism.Count_data.incr_real_import cd ;
     LP.LP_printer.pp_command ff import_trans ;
     (* STEP 3: Translate the executable *)
     let p_exec, free_var_data = Translating.Executable.iter_exec exec empty_sign in
     (* STEP 4: Print free variables *)
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
     (* STEP 5: Translate the result of the executable *)
     let p_res, _ = Translating.Executable.iter_exec result empty_sign in
     (* STEP 6: Print the symbol s_e which represents the executable *)
     LP.LP_printer.pp_command ff
       (Interface.LP_p_term.create_LP_symbol
          (Interface.LP_p_term.create_symbol_with_body "PGM" p_exec)) ;
     (* STEP 7: Print the symbol s_r which represents the result *)
     LP.LP_printer.pp_command ff
       (Interface.LP_p_term.create_LP_symbol
          (Interface.LP_p_term.create_symbol_with_body "RES" p_res)) ;
     (* STEP 8: Print the assert command to check s_e == s_r *)
     let s_e = Interface.LP_p_term.create_ident "PGM" in
     let s_r = Interface.LP_p_term.create_ident "RES" in
     LP.LP_printer.pp_command ff
       (Interface.LP_p_term.create_assert_command s_e s_r) ;
     (* STEP 9: Close the new file *)
     Format.pp_print_flush ff ();
     close_out f ;
     flush stdout
  | F_sem (_, file) ->
     (* STEP 1: Pre-processing *)
        (* a. Create the new file *)
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
        (* b. Printing management *)
     let printing = match !Terminal.Cmd_line.output with
       | O_LP      -> LP.LP_printer.pp_command
       | O_Dedukti -> fun _ _ -> () (* @TODO *)
       | O_Kore    -> fun _ _ -> () (* Printer.pp_kore_kommand ff cd *)
     in

     (* STEP 2: The main function to translate one Kore module *)
     let module_to_file : signature -> kmodule -> signature =
       fun sign m ->
       (* let name, import_l, command_l, attribut_l = m in *)
       let name, _, kommand_l, _ = m in
          (* a. Cleaning the semantic files *)
       let kommand_l =
         if not(name = "BASIC-K" || name = "KSEQ" || name = "INJ" || name = "K") then
           Controller.Cleaning.cleaning kommand_l
         else kommand_l
       in
       Common.Error.print ff "\n// Translation of the module ";
       Format.pp_print_string ff name;
       Common.Error.print ff "\n";
          (* b. Reset count data *)
       let cd = Mecanism.Count_data.reset_count_data 0 in
          (* c. Translation management *)
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
                  | O_Kore ->
                     Printing.Kore_printer.pp_kore_kommand ff cd kommand_l)
              | M_K       ->
                 Printing.Meta_printer.prt_Viry ff cd printing res
              | M_Dedukti -> ()) ; new_sign
           end
       in
          (* d. Printing count data *)
       print_module_message name (List.length kommand_l) cd;
       Format.pp_print_flush ff () ; sign_bis
     in

     (* STEP 3: Run the main translation *)
        (* a. Translation of BASIC-K module *)
     print_header_kamelo ();
     let sign_g = module_to_file empty_sign (List.hd file) in
        (* b. Printing of the K prelude interface *)
     print_comment ff "PRELUDE";
     let sign_res =
       Controller.Prelude.create_prelude ff printing sign_g "prelude"
     in
        (* c. Translation of KSEQ, INJ, K modules, and the semantic *)
     let _ = List.fold_left module_to_file sign_res (List.tl file) in
     print_footer_kamelo ();

     (* STEP 4: Close the new file *)
     close_out f;
     flush stdout;;
