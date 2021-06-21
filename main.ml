
open Arg

open Type

open Axiom
open Symbol

open Display_console
open Output
open Printer

open Preprocessing

let input = ref stdin
let c_prefix = ref "a.out"

let old = ref false

let basename s =
  try String.sub s 0 (String.rindex s '.')
  with Not_found -> s

let check_extension s =
  let len = String.length s in
  if len > 6 then
    (if not (s.[len-5] = '.' && s.[len-4] = 'k' && s.[len-3] = 'o' && s.[len-2] = 'r' && s.[len-1] = 'e') then
       raise (Invalid_argument "Expected extension: .kore"))
  else
    raise (Invalid_argument "Name file very short")

let lp_pkg = "tests"
let prelude_path = ["Tests"] (* depuis lp_pkg *)
let prelude_name = "prelude"

let get_filename name =
  let tmp = String.lowercase_ascii name in
  if !lp_output then tmp ^ ".lp" else tmp ^ ".dk"


type output_management = K | Kore | Dedukti

let set_format o = if o = "K" || o = "k" then k_format := true
                   else
                     if o = "Kore" || o = "kore" then kore_format := true
                     else
                       if o = "Dedukti" || o = "dedukti" then dk_format := true
                       else failwith ("The option" ^ o ^ "is unknow")

let set_output o = if o = "Dedukti" || o = "dedukti" then lp_output := false
                   else
                     if o = "Lambdapi" || o = "lambdapi" then lp_output := true
                     else failwith ("The option"^ o ^ "is unknow")
let () =
  let usage_msg = "usage: ./kamelo [-f (K|Kore|Dedukti)] [-o (Dedukti|Lambdapi)] [--inductive] [--readable] [--no-color] kore_file" in
  parse
    [("--format",  String (fun o -> set_format o),  "Change the ordering of commands");
     ("-f",  String (fun o -> set_format o),  "Change the ordering of commands");
     ("--output",  String (fun o -> set_output o), "Change the output: .dk file or .lp file");
     ("-o", String (fun o -> set_output o), "Change the output: .dk file or .lp file");

     ("--inductive",  Unit (fun () -> check_induc:=true),  "Use inductive types");
     ("-i",           Unit (fun () -> check_induc:=true),  "Use inductive types");

     ("--old",  Unit (fun () -> old:=true),  "Use the old preprocessing algorithm");

     ("--readable",  Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-r",  Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     (*("-v",  Unit (fun () -> verbose:=1), "reports stuff");
     ("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color",  Unit (fun () -> no_color:=true),  "Disable colors on the main message")]
    (fun s ->
      check_extension s;
      c_prefix := basename s;
      input := open_in s)
    ("During compilation of a .kore program:\n" ^ usage_msg)  (* Format.printf "%s" usage_msg "During compilation of a .kore program"*)

let () =
  let lexbuf = Lexing.from_channel (!input) in
  let file = Kparser.file Klexer.token lexbuf in
  (*let rec print_c c = match c with
    | [] -> Format.fprintf Format.std_formatter "\n"
    | a::t -> Format.fprintf Format.std_formatter "%a" pp_command a; print_c t
  in
  print_c c;*)

  (* let param_to_p_params : param -> p_params = fun p -> *)





  (*let trans_axiom : Format.formatter -> axiom -> unit =*)

  (* let trans_command : Format.formatter -> command -> unit =
    fun ppf (c, attri_l) ->
    match c with
     | Sort   s | H_sort   s ->
        pp_command ppf (Pos.none (P_symbol (sort_to_p_symbol s)))
     | Symbol s | H_symbol s ->
        pp_command ppf (Pos.none (P_symbol (symbol_to_p_symbol s attri_l)))
     | Alias  _       -> ()
     | Axiom  (qv, a) ->
        match attri_l with
         | Unit _::nil | Comm _::nil | Assoc _::nil | Idem _::nil ->
            pp_command ppf (Pos.none (P_rules [of_equality_axiom a]))
         | _ -> ()
  in *)





  let module_to_file : kmodule -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    let len = List.length in

    let cd = reset_count_data 0 in

    let name, kimport_l, kcommand_l, _ = m in
    cd.k_import := len (kimport_l) ;
    let filename = get_filename name in
    print_header_file filename;

    print_nb_total_commands (len kcommand_l);

    let _, import_l, sort_l, induc_m, sym_l, alias_l, ax_l =
      if !old then Preprocessing.preprocessing m cd
      else "truc", [], [], Induc.empty, [], [], []
    in

    (* let import_l = if Induc.is_empty induc_m then import_l else ("prelude", [])::import_l in *)

    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in

    List.iter (pp_import ff cd [lp_pkg]) import_l;
    pp_import ff  cd (lp_pkg::prelude_path) (prelude_name, []);
    List.iter (pp_sort ff cd) sort_l;
    List.iter (pp_induc ff cd) (List.rev (Induc.bindings induc_m));
    List.iter (pp_symbol ff cd) sym_l;
    (*List.iter (trans_command ff) command_l;*)
    List.iter (pp_alias ff cd) alias_l;
    List.iter (pp_axiom ff cd) ax_l;
    (*List.iter (trans_command Format.std_formatter) command_l;*)

    print_count_data cd;

    print_separator ();
    Format.pp_print_flush ff ();
    close_out f
  in
  print_header_kamelo ();
  List.iter module_to_file (snd file);
  print_footer_kamelo ();
  flush stdout;;
