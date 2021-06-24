open Arg

open Color
open Output
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

let k_format    = ["K"; "k"]
let kore_format = ["Kore"; "kore"]
let dk_format   = ["DK"; "Dedukti"; "Dk"; "dedukti"; "dk"]

let set_format o = if List.mem o k_format then format := K
                   else
                     if List.mem o kore_format then format := Kore
                     else
                       if List.mem o dk_format then format := Dedukti
                       else failwith ("The option" ^ o ^ "is unknow")

let dk_output = dk_format
let lp_output = ["LP"; "Lambdapi"; "lambdapi"; "lp"; "Dedukti3"; "dedukti3"; "DK3"; "Dk3"; "dk3"]

let set_output o = if List.mem o dk_output then output := Dedukti
                   else
                     if List.mem o lp_output then output := LP
                     else failwith ("The option"^ o ^ "is unknow")

let parse : unit -> unit = fun () ->
  let usage_msg = "usage: ./kamelo [--format (K|Kore|Dedukti)] [-o (Dedukti|Lambdapi)] [--inductive] [--readable] [--no-color] kore_file" in
  parse
    [("--format", String (fun o -> set_format o), "Change the ordering of commands");
     ("-f",       String (fun o -> set_format o), "Change the ordering of commands");
     ("--output", String (fun o -> set_output o), "Change the output: .dk file or .lp file");
     ("-o",       String (fun o -> set_output o), "Change the output: .dk file or .lp file");

     ("--inductive", Unit (fun () -> check_induc:=true),  "Use inductive types");
     ("-i",          Unit (fun () -> check_induc:=true),  "Use inductive types");

     ("--old",  Unit (fun () -> old:=true),  "Use the old preprocessing algorithm");

     ("--readable", Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-r",         Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     (*("-v",  Unit (fun () -> verbose:=1), "reports stuff");
     ("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color", Unit (fun () -> no_color:=true),  "Disable colors on the main message")]
    (fun s ->
      check_extension s;
      input := open_in s)
    ("During compilation of a .kore program:\n" ^ usage_msg)
