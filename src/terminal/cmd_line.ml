open Arg

open Common.Color
open Printing.Kore_printer
open Interface.Output (* TODO remove thanks to DiaLeKTo *)

open Controller.Preprocessing

(** Some specific type for references *)
type mimic_management  = M_K  | M_Kore | M_Dedukti
type output_management = O_LP | O_Kore | O_Dedukti


(** References for managing the options *)
let mimic          = ref M_Kore
let output         = ref O_LP
let filename_exec  = ref ""
let semantics_file = ref ""
let input          = ref stdin
let old            = ref false

(** Utilities *)

(** [create_filename name] creates a filename
    with name in lowercase the extension ".dk",
    ".lp" or ".mykore" according to !output. *)
let create_filename name =
  let tmp = String.lowercase_ascii name in
  match !output with
  | O_Dedukti -> tmp ^ ".dk"
  | O_LP      -> tmp ^ ".lp"
  | O_Kore    -> tmp ^ ".mykore"

(** [check_extension s] checks that the input file has the
    extension ".kore". *)
let check_extension s =
  let len = String.length s in
  if len > 5 then
    (if not (String.sub s ((String.length s)-5) 5 = ".kore")
     then
       raise (Invalid_argument "Expected extension: .kore"))
  else
    raise (Invalid_argument "Name file very short")



let k_mimic    = ["K"; "k"]
let kore_mimic = ["Kore"; "kore"]
let dk_mimic   = ["DK"; "Dedukti"; "Dk"; "dedukti"; "dk"]

let set_mimic o =
  if List.mem o k_mimic then mimic := M_K
  else
    if List.mem o kore_mimic then mimic := M_Kore
    else
      if List.mem o dk_mimic then mimic := M_Dedukti
      else failwith ("The option" ^ o ^ "is unknow.")

let dk_output = [".dk"]
let lp_output = [".lp"] (* ["LP"; "Lambdapi"; "lambdapi"; "lp"; "Dedukti3"; "dedukti3"; "DK3"; "Dk3"; "dk3"] *)
let kore_output = [".mykore"]

let set_output o =
  if List.mem o dk_output then output := O_Dedukti
  else
    if List.mem o kore_output then output := O_Kore
    else
      if List.mem o lp_output then output := O_LP
      else failwith ("The option"^ o ^ "is unknow")

let set_semantics s = semantics_file := s

(** [parse ()] parses the command line. *)
let parse : unit -> unit = fun () ->
  let usage_msg =
    "usage: ./KaMeLo [--semantics (.lp|.dk)]
     [--mimic (K|Kore|Dedukti)] [-o (Dedukti|Lambdapi|Kore)]
     [--inductive] [--readable] [--no-color] kore_file"
  in
  parse
    [("--semantics",  String (fun o -> set_semantics o),
      "Select the semantics to run the input programm");
     ("-s",       String (fun o -> set_semantics o),
      "Select the semantics to run the input programm");
     ("--mimic",  String (fun o -> set_mimic o),
      "Mimic the format of K, Kore or Dedukti, especially the ordering
       of commands");
     ("-m",       String (fun o -> set_mimic o),
      "Mimic the format of K, Kore or Dedukti, especially the ordering
       of commands");
     ("--output", String (fun o -> set_output o),
      "Generate files with the extension .dk, .lp or .mykore");
     ("-o",       String (fun o -> set_output o),
      "Generate files with the extension .dk, .lp or .mykore");

     ("--inductive", Unit (fun () -> check_induc:=true),
      "Use inductive types");
     ("-i",          Unit (fun () -> check_induc:=true),
      "Use inductive types");

     ("--old",  Unit (fun () -> old:=true),
      "Use the old preprocessing algorithm");

     ("--readable", Unit (fun () -> readable:=true),
      "Generate identifiers more readable");
     ("-r",         Unit (fun () -> readable:=true),
      "Generate identifiers more readable");
     ("-v",  Unit (fun () -> verbose:=true),
      "Print #Var and #Sym in Kore output mode");
     (*("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color", Unit (fun () -> no_color:=true),
      "Disable colors on the main message")]
    (fun s ->
      check_extension s;
      filename_exec := String.sub s 0 ((String.length s)-5);
      input := open_in s)
    ("During compilation of a .kore program:\n" ^ usage_msg)
