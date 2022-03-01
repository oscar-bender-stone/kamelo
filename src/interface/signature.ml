open Common.Xlib_OCaml
open Common.Type
open LP.Syntax

module Sort = struct
  type t = sort
  let compare = String.compare
end
module Induc = Map.Make(Sort)  (* ( sort |-> symbol list) *)

type signature =
  { typing    : p_term StrMap.t        ; (** To store the type [value]
                                             of each symbol [key].      *)
    subsort   : (string list) StrMap.t ; (** where the key is a subsort
                                             of the listed sorts        *)
    inductive : (symbol list) Induc.t  }

let empty_sign =
  { typing    = StrMap.empty ;
    subsort   = StrMap.empty ;
    inductive = Induc.empty  }

(** [add_update key value m] adds the value [value] at the entry [key] in
    the map [m]. *)
let add_update_induc : string -> 'a -> ('a list) Induc.t -> ('a list) Induc.t =
  fun key value m ->
  let f a = match a with
    | None   -> Some [value]   (* If the key did not yet exist *)
    | Some l -> Some(value::l) (* If the key already existed   *)
  in
  Induc.update key f m
