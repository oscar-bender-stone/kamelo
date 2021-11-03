open Common.Type
open LP.Syntax

val has_no_param : symbol -> bool

val get_modifier : attribute list -> p_modifier list

val get_param : symbol -> param
val get_sort : symbol -> sort
val get_name : symbol -> name
val is_constructor : symbol -> attribute list -> sort option
val get_type : string -> p_term

val sym_curry : symbol -> p_term

val def_to_p_term : def -> p_term

val param_to_p_term : param -> p_term

(** [create_p_params_expl l] creates explicit parameters, which have the current given type,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
val create_p_params_expl : (name * param) list -> p_params list

(** [create_p_params s_l] creates implicit parameters, which have the type _SORTK,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
val create_p_params : string list -> p_params list

val alias_to_definition : alias -> p_command
