
type mimic_management = K | Kore | Dedukti
val mimic : mimic_management ref

type output_management = LP | Dedukti | Kore
val output : output_management ref

val create_filename : string -> string


val verbose : bool ref

val filename_exec : string ref
val semantics_file : string ref
val input : in_channel ref
val old : bool ref

(** [check_extension s] checks that the input file has the extension .kore. *)
val check_extension : string -> unit

(** [parse ()] parses the command line. *)
val parse : unit -> unit
