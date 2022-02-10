
(** Some specific type for references *)
type mimic_management  = M_K  | M_Kore | M_Dedukti
type output_management = O_LP | O_Kore | O_Dedukti


(** References for managing the options *)
val mimic          : mimic_management ref
val output         : output_management ref
val filename_exec  : string ref
val semantics_file : string ref
val input          : in_channel ref
val old            : bool ref

(** Utilities *)

(** [create_filename name] creates a filename
    with name in lowercase the extension ".dk",
    ".lp" or ".mykore" according to !output. *)
val create_filename : string -> string

(** [check_extension s] checks that the input file has the
    extension ".kore". *)
val check_extension : string -> unit

(** [parse ()] parses the command line. *)
val parse : unit -> unit
