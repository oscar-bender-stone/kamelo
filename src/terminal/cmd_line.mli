
(** Some specific type for references *)
type logic_option      = Interpreted | MSML
type mimic_management  = M_K  | M_Kore | M_Dedukti
type output_management = O_LP | O_Dedukti

(** References for managing the options *)
val logic_used     : logic_option ref
val mimic          : mimic_management ref
val output         : output_management ref
val debug          : bool ref
val input_filename : string ref
val semantics_file : string ref (* given by --semantics *)
val krun_result    : string ref (* given by --result    *)
val trace_file     : string ref (* given by --trace     *)
val input          : in_channel ref
val old            : bool ref

(** Utilities *)

(** [create_filename name] creates a filename
    with name in lowercase the extension ".dk",
    or ".lp" according to !output. *)
val create_filename : string -> string

(** [check_extension s] checks that the input file has the
    extension ".kore". *)
val check_extension : string -> unit

(** [parse ()] parses the command line. *)
val parse : unit -> unit
