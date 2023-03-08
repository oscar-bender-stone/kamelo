(** Translating importation *)

open LP.Syntax
open Common.Type
open Interface.LP_p_term

(** [import_to_require_open path i] translates a Kore import to a
    "require open" command, with only one path and without position. *)
let import_to_require_open : string list -> import -> p_command =
  fun path i ->
  let filename = String.lowercase_ascii (fst i)  in
  let path = [create_p_path (path @ [filename])] in
  no_pos (P_require (true, path))
