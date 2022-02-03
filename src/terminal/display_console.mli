
(** [print_header_kamelo] prints the header message when running KaMeLo. *)
val print_header_kamelo : unit -> unit

(** [print_footer_kamelo] prints the footer message when running KaMeLo. *)
val print_footer_kamelo : unit -> unit

(** [print_module_message f nb cd] prints a review of the translation
    of the file named [f], which has [nb] commands.
    The current data useful for the review are in [cd]. *)
val print_module_message :
  string -> int -> Mecanism.Count_data.count_data -> unit
