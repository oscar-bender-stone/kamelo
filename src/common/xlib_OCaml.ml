
module StrMap = Map.Make(String)

(** [add_update key value m] adds the value [value] at the entry [key] in the map [m]. *)
let add_update : string -> 'a -> ('a list) StrMap.t -> ('a list) StrMap.t = fun key value m ->
  let f a = match a with
    | None   -> Some [value]   (* Si l'entrée n'existait pas encore *)
    | Some l -> Some(value::l) (* Si l'entrée existait déjà *)
  in
  StrMap.update key f m

(** [add_update_without_dup key value m] adds the value [value] at the entry [key] in the map [m].
    The value [value] it is added only if it was not already in the list associated with [key]. *)
let add_update_without_dup : string -> 'a -> ('a list) StrMap.t -> ('a list) StrMap.t = fun key value m ->
  let f a = match a with
    | None   -> Some [value]                                        (* Si l'entrée n'existait pas encore *)
    | Some l -> if List.mem value l then Some l else Some(value::l) (* Si l'entrée existait déjà *)
  in
  StrMap.update key f m
