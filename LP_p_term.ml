
open Syntax
open Output

let _SORTK = "SortK"
let _INJ = "injK"
let _INJGEN = "injG"

(** [no_pos alpha] creates nothing without position. *)
let no_pos = Pos.none

let create_prop : prop -> p_modifier = fun p -> no_pos (P_prop p)

(** [create_path s_l] creates a path thanks to a list of string. *)
let create_path : string list -> path = fun x -> x

(** [create_qident x] creates a qualified identifier thanks to a path
    and a name. *)
let create_qident : path * string -> qident = fun x -> x

(** [create_p_ident s] creates an identifier without position. *)
let create_p_ident : string -> p_ident = fun s -> no_pos s

(** [create_p_path x] creates a path without position. *)
let create_p_path : string list -> p_path = fun x ->
  no_pos (create_path x)

(** [create_p_qident x] creates a qualified identifier without position. *)
let create_p_qident : path * string -> p_qident = fun x ->
  no_pos (create_qident x)

let create_meta_var : string -> p_meta_ident = fun s -> no_pos (Name s)

(** P_term *)

(** [_TYPE] is the constant TYPE without position. *)
let _TYPE : p_term = no_pos P_Type

(** [create_ident s] creates an identifier (just a name) without position.
    This identifier is not prefixed by "@". *)
let create_ident : string -> p_term = fun s ->
  let tmp = create_p_qident ([], (pp s)) in
  no_pos (P_Iden(tmp, false))

(** [_WILD] is the constant "_" without position. *)
let _WILD : p_term = no_pos P_Wild

(** [create_meta s] creates a meta-variable without argument and position. *)
let create_meta : string -> p_term = fun s ->
  no_pos (P_Meta (create_meta_var s, None))

(** [create_pattern_var s] creates a variable of pattern without argument and
    position. *)
let create_pattern_var : string -> p_term = fun s ->
  let name = Some (create_p_ident (pp s)) in
  no_pos (P_Patt(name, None))

  (** [create_appl a1 a2] creates the application of [a1] on [a2], without
      position. *)
let create_appl : p_term -> p_term -> p_term = fun a1 a2 ->
  no_pos (P_Appl(a1, a2))

(** [create_arrow t1 t2] creates the type [t1 -> t2], without position. *)
let create_arrow : p_term -> p_term -> p_term = fun t1 t2 ->
  no_pos (P_Arro(t1, t2))

(** [create_implicit_arg s] creates an identifier (just a name) as
    implicit argument, without position. *)
let create_implicit_arg : string -> p_term = fun s ->
  no_pos (P_Expl(create_ident s))
(* P_Expl -> P_Impl? @TODO *)

(** [create_p_params s_l] creates implicit parameters, which have the type _SORTK,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params : string list -> p_params list = fun s_l ->
  match s_l with
  | []   -> []
  | _::_ ->
     let unique_name s = Some (no_pos s)  in
     let typ = Some (create_ident _SORTK) in
     let is_implicit = true in
     [ List.map unique_name s_l, typ, is_implicit ]
