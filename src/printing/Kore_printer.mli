
(*  *type output = Format.formatter *)

open Mecanism.Count_data
open Common.Type

val verbose : bool ref

val pp_kore_axiom : Format.formatter -> int -> axiom -> unit

val pp_kore_hooked_sort : Format.formatter -> sort -> attribute list -> unit
val pp_kore_import  : Format.formatter -> import -> unit
val pp_kore_kommand : Format.formatter -> count_data -> kommand list -> unit
