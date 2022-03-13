open Common.Type
open Interface.Signature

open Axiom

(** ---------------------------- *)
(** To translate heating rules   *)
(** ---------------------------- *)

val trans_heating_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list

(** ---------------------------- *)
(** To translate cooling rules   *)
(** ---------------------------- *)

val trans_cooling_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list

(** ---------------------------- *)
(** To translate semantic rules  *)
(** ---------------------------- *)

val trans_semantic_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list
