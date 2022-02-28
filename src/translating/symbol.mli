open Common.Type
open LP.Syntax

val get_modifier : attribute list -> p_modifier list
val sym_curry : symbol -> p_term
val symbol_to_p_symbol : symbol -> attribute list -> p_command
