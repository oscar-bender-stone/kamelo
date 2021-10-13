open Arg

open Common.Color
open LP_interface.Output
open Printing.Printer
open Preprocessing

let input = ref stdin
let old = ref false

(** [check_extension s] checks that the input file has the extension .kore. *)
let check_extension s =
  let len = String.length s in
  if len > 6 then
    (if not (s.[len-5] = '.' && s.[len-4] = 'k' && s.[len-3] = 'o' && s.[len-2] = 'r' && s.[len-1] = 'e') then
       raise (Invalid_argument "Expected extension: .kore"))
  else
    raise (Invalid_argument "Name file very short")

let k_mimic    = ["K"; "k"]
let kore_mimic = ["Kore"; "kore"]
let dk_mimic   = ["DK"; "Dedukti"; "Dk"; "dedukti"; "dk"]

let set_mimic o = if List.mem o k_mimic then mimic := K
                   else
                     if List.mem o kore_mimic then mimic := Kore
                     else
                       if List.mem o dk_mimic then mimic := Dedukti
                       else failwith ("The option" ^ o ^ "is unknow.")

let dk_output = [".dk"]
let lp_output = [".lp"] (* ["LP"; "Lambdapi"; "lambdapi"; "lp"; "Dedukti3"; "dedukti3"; "DK3"; "Dk3"; "dk3"] *)
let kore_output = [".mykore"]

let set_output o = if List.mem o dk_output then output := Dedukti
                   else
                     if List.mem o kore_output then output := Kore
                     else
                       if List.mem o lp_output then output := LP
                       else failwith ("The option"^ o ^ "is unknow")

let parse : unit -> unit = fun () ->
  let usage_msg = "usage: ./KaMeLo [--mimic (K|Kore|Dedukti)] [-o (Dedukti|Lambdapi|Kore)] [--inductive] [--readable] [--no-color] kore_file" in
  parse
    [("--mimic",  String (fun o -> set_mimic o), "Mimic the format of K, Kore or Dedukti, especially the ordering of commands");
     ("-m",       String (fun o -> set_mimic o), "Mimic the format of K, Kore or Dedukti, especially the ordering of commands");
     ("--output", String (fun o -> set_output o), "Generate files with the extension .dk, .lp or .mykore");
     ("-o",       String (fun o -> set_output o), "Generate files with the extension .dk, .lp or .mykore");

     ("--inductive", Unit (fun () -> check_induc:=true),  "Use inductive types");
     ("-i",          Unit (fun () -> check_induc:=true),  "Use inductive types");

     ("--old",  Unit (fun () -> old:=true),  "Use the old preprocessing algorithm");

     ("--readable", Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-r",         Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-v",  Unit (fun () -> verbose:=true), "Print #Var and #Sym in Kore output mode");
     (*("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color", Unit (fun () -> no_color:=true),  "Disable colors on the main message")]
    (fun s ->
      check_extension s;
      input := open_in s)
    ("During compilation of a .kore program:\n" ^ usage_msg)
