
val filename_exec : string ref
val semantics_file : string ref
val input : in_channel ref
val old : bool ref

(** [check_extension s] checks that the input file has the extension .kore. *)
val check_extension : string -> unit

(** [parse ()] parses the command line. *)
val parse : unit -> unit
