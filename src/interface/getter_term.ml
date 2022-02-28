open K_prelude
open LP_p_term
open LP.Syntax
open Common.Type
open Common.Error

(** [p_INJD_appl_ident s] creates the term δ s. *)
let p_INJD_appl_ident : string -> p_term = fun s ->
  create_appl p_INJD (create_ident s)

let wrap = p_INJD_appl_ident

(** [get_sort_type s] creates the type :
      - p_TYPE       if s = _SORTK
      - p_SORTK      otherwise *)
let get_sort_type : sort -> p_term = fun s ->
  if s = _SORTK then p_TYPE else p_SORTK

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
    | []  -> raise (InternalError ("The symbol " ^ name ^ " has no type."))
    | [t]  -> List.rev acc, t
    | t::q -> split_last_value q (t::acc)
  in
  let head, last = split_last_value type_l [] in
  let f t1 t2 = create_arrow (create_type_atomic t1) t2 in
  List.fold_right f head (create_type_atomic last)


let param_to_p_term p = match p with
  | S s -> create_type_atomic s
  | Q _ -> p_TYPE

(** [create_p_params s_l] creates implicit parameters, which have the
    type _SORTK, without position.
    Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params : string list -> p_params list = fun s_l ->
  match s_l with
  | []   -> []
  | _::_ ->
     let unique_name s = Some (no_pos s)  in
     let typ = Some p_SORTK in
     let is_implicit = true in
     [ List.map unique_name s_l, typ, is_implicit ]

(** [create_p_params_expl l] creates explicit parameters, which have the
    current given type, without position.
    Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params_expl : (name * param) list -> p_params list = fun s_l ->
  let is_implicit = false in
  let f (n,p) =
    ([Some (create_p_ident n)], Some (param_to_p_term p), is_implicit)
  in
  List.map f s_l

(*
  val param_to_p_term : param -> p_term
  val create_p_params_expl : (name * param) list -> p_params list
  val create_p_params : string list -> p_params list
 *)
