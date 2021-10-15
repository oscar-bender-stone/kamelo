open Arg

(** [check_extension s] checks that the input file has the extension .kore. *)
let check_extension s =
  let len = String.length s in
  if len > 6 then
    (if not (s.[len-5] = '.' && s.[len-4] = 'k' && s.[len-3] = 'o' && s.[len-2] = 'r' && s.[len-1] = 'e') then
       raise (Invalid_argument "Expected extension: .kore"))
  else
    raise (Invalid_argument "Name file very short")

let semantics_file = ref ""

let set_semantics s = semantics_file := s

let parse : unit -> unit = fun () ->
  let usage_msg = "usage: ./dkrun [--semantics (.lp|.dk)] kore_file" in
  parse
    [("--semantics",  String (fun o -> set_semantics o), "Select the semantics to run the input programm");
     ("-s",       String (fun o -> set_semantics o), "Select the semantics to run the input programm");
     (* ("--output", String (fun o -> set_output o), "Generate files with the extension .dk, .lp or .mykore");
     ("-o",       String (fun o -> set_output o), "Generate files with the extension .dk, .lp or .mykore");

     ("--inductive", Unit (fun () -> check_induc:=true),  "Use inductive types");
     ("-i",          Unit (fun () -> check_induc:=true),  "Use inductive types");

     ("--old",  Unit (fun () -> old:=true),  "Use the old preprocessing algorithm");

     ("--readable", Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-r",         Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-v",  Unit (fun () -> verbose:=true), "Print #Var and #Sym in Kore output mode");
     (*("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color", Unit (fun () -> no_color:=true),  "Disable colors on the main message")*)]
    (fun s ->
      check_extension s;
      Terminal.Cmd_line.input := open_in s)
    ("During compilation of a .kore program:\n" ^ usage_msg)


open LP_interface.Output

let () =
  (* STEP A: Parse the command-line *)
  parse ();
  (* STEP B: Parse the .kore file   *)
  let lexbuf = Lexing.from_channel (!Terminal.Cmd_line.input) in
  let exec = Run_parser.exec Run_lexer.token lexbuf in
  (* STEP C: Create the new file *)
  let name = "exec_dkrun" in
  let filename = get_filename name in
  let f  = open_out filename in
  let ff = Format.formatter_of_out_channel f in
  (* STEP D: Translate the executable *)
  LP_interface.LP_printer.pp_p_term ff (Translation.Axiom.curry_ident exec) ;
  (* STEP E: Close the new file *)
  Format.pp_print_flush ff ();
  close_out f ;
  flush stdout;;
