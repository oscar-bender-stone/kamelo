open Arg

open Common.Color
open Printing.Kore_printer
open Printing.Meta_printer
open Interface.Output (* TODO remove thanks to DiaLeKTo *)

open Controller.Old

(** Some specific type for references *)
type logic_option      = Interpreted | MSML
type mimic_management  = M_K  | M_Kore | M_Dedukti
type output_management = O_LP | O_Dedukti

(** References for managing the options *)
let logic_used     = ref Interpreted
let mimic          = ref M_Kore
let output         = ref O_LP
let debug          = ref false
let input_filename = ref ""
let semantics_file = ref ""      (* given by --semantics *)
let krun_result    = ref ""      (* given by --result    *)
let trace_file     = ref ""      (* given by --trace     *)
let input          = ref stdin
let old            = ref false

(** Utilities *)

(** [create_filename name] creates a filename
    with name in lowercase the extension ".dk",
    or ".lp" according to !output. *)
let create_filename name =
  let tmp = String.lowercase_ascii name in
  match !output with
  | O_Dedukti -> tmp ^ ".dk"
  | O_LP      -> tmp ^ ".lp"

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
      else raise (Invalid_argument ("The option " ^ o ^ " is unknow."))

let dk_output = [".dk"]
let lp_output = [".lp"] (* ["LP"; "Lambdapi"; "lambdapi"; "lp"; "Dedukti3"; "dedukti3"; "DK3"; "Dk3"; "dk3"] *)

let set_output o =
  if List.mem o dk_output then output := O_Dedukti
  else
    if List.mem o lp_output then output := O_LP
    else raise (Invalid_argument ("The option "^ o ^ " is unknow"))

let set_logic o =
  if o = "Interpreted" then ()
  else
    if o = "ManySorted" then logic_used := MSML
    else raise (Invalid_argument ("The option "^ o ^ " is unknow"))

let set_semantics o = semantics_file := o
let set_result    o = krun_result := o
let set_trace     o = trace_file := o


(** [parse ()] parses the command line. *)
let parse : unit -> unit = fun () ->
  let usage_msg =
    "
 * Usage for translating a K semantics:
     - in order to execute it:
         ./KaMeLo kore_file [--logic interpreted]
     - to many-sorted Matching Logic:
         ./KaMeLo kore_file  --logic ManySorted
   Additional options during the translation: --mimic, --inductive, --old, --lib

 * Usage for translating a program in order to execute it:
     ./KaMeLo kore_file  --semantics sem_file_name
                        [--logic interpreted]
                        [--result kore_file]

 * Usage for translating a program specification to many-sorted Matching Logic:
     ./KaMeLo kore_file  --semantics sem_file_name
                         --logic ManySorted
                        [--trace yaml_file]

 * Printing options: --output (.dk|.lp), --readable, --no-color

 * Usage for debugging: ./KaMeLo kore_file --debug [-v]

 MORE DETAILS ABOUT THE OPTIONS:"
  in
  parse
    [("--logic",  String (fun o -> set_logic o),
      "Give the logic to encode the K semantics.");
     ("-l",       String (fun o -> set_logic o),
      "Give the logic to encode the K semantics.\n");
     ("--mimic",  String (fun o -> set_mimic o),
      "Mimic the format of K, Kore or Dedukti, especially the ordering
       of commands. Should be K, Kore or Dedukti.");
     ("-m",       String (fun o -> set_mimic o),
      "Mimic the format of K, Kore or Dedukti, especially the ordering
       of commands. Should be K, Kore or Dedukti.\n");
     ("--inductive", Unit (fun () -> check_induc:=true),
      "Use inductive types during the translation process.");
     ("-i",          Unit (fun () -> check_induc:=true),
      "Use inductive types during the translation process.\n");
     ("--old",  Unit (fun () -> old:=true),
      "Use the old preprocessing algorithm.");
     ("--lib",  Unit (fun () -> lib:=true),
      "Generate a unique file that contains also the K standard library.\n");

     ("--semantics", String (fun o -> set_semantics o),
      "Give the semantics used to write the input file.");
     ("-s",          String (fun o -> set_semantics o),
      "Give the semantics used to write the input file.\n");

     ("--result", String (fun o -> set_result o),
      "Give the result of krun when executing the input file.");

     ("--trace", String (fun o -> set_trace o),
      "Give the trace provided by the KProver from the input file.");

     ("--output", String (fun o -> set_output o),
      "Generate files with the extension .dk or .lp.");
     ("-o",       String (fun o -> set_output o),
      "Generate files with the extension .dk or .lp.\n");

     ("--readable", Unit (fun () -> readable:=true),
      "To print identifiers more readable.");
     ("-r",         Unit (fun () -> readable:=true),
      "To print identifiers more readable.\n");
     (*("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color", Unit (fun () -> no_color:=true),
      "Disable colors on the main message.");

     ("--debug", Unit (fun () -> debug:=true),
      "Print the input Kore file.");
     ("-v",  Unit (fun () -> verbose:=true),
      "Print #Var and #Sym in Kore output mode.\n")]
    (fun s ->
      check_extension s;
      input_filename := String.sub s 0 ((String.length s)-5);
      input := open_in s)
    ("  -- DOCUMENTATION OF KAMELO'S COMMAND-LINE OPTIONS --\n" ^ usage_msg)
