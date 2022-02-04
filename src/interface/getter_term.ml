open K_prelude
open LP_p_term
open LP.Syntax

(** [create_type_atomic s] creates the type :
      - _SORTK       if s = _SORTK
      - p_INJD (f s) otherwise
    Note: f transforms s into a p_term. *)
let create_type_atomic : string -> p_term = fun s ->
  let p_s = create_ident s in
  if s = _SORTK then p_s else create_appl p_INJD p_s

(** [create_type_arrow (name, type_l)] creates the type :
      - ~t                                if type_l = [t]
      - ~t1 -> ( ... -> (~t(n-1) -> ~tn)) if type_l = [t1;...;tn]
      - raise an error                    otherwise
    Note: ~ add the injection p_INJD if ti <> _SORTK
          (See [create_type_atomic]). *)
let create_type_arrow : string * string list -> p_term =
  fun (name, type_l) ->
  let rec split_last_value l acc = match l with
    | []  -> failwith ("The symbol " ^ name ^ " has no type.")
    | [t]  -> List.rev acc, t
    | t::q -> split_last_value q (t::acc)
  in
  let head, last = split_last_value type_l [] in
  let f t1 t2 = create_arrow (create_type_atomic t1) t2 in
  List.fold_right f head (create_type_atomic last)
