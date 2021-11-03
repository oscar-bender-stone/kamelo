
open LP.Syntax
open Output

let _SORTK = "SortK"
let _INJD  = "δ"
let _INJ   = "inj"
let _KSEQ  = "kseq"
let _DOTK  = "dotk"

let _TRUE  = "true"
let _FALSE = "false"

(** [no_pos alpha] creates nothing without position. *)
let no_pos = LP.Pos.none

(** [create_prop p] creates a modifier with one property [p]. *)
let create_prop : prop -> p_modifier = fun p -> no_pos (P_prop p)

(** [create_path s_l] creates a path thanks to a list of string.
    Note: The option -r can't change the name. *)
let create_path : string list -> path = fun x -> x

(** [create_p_path x] creates a path without position.
    Note: The option -r can't change the name. *)
let create_p_path : string list -> p_path = fun x ->
  no_pos (create_path x)

(** [create_qident (p, s)] creates a qualified identifier thanks to
    a path [p] and a name [s]. Note: The option -r can change the name. *)
let create_qident : path * string -> qident = fun (p, s) -> (p, pp s)

(** [create_p_qident x] creates a qualified identifier without position.
    Note: The option -r can change the name. *)
let create_p_qident : path * string -> p_qident = fun x ->
  no_pos (create_qident x)

(** [create_p_ident s] creates an identifier without position.
    Note: The option -r can change the name. *)
let create_p_ident : string -> p_ident = fun s -> no_pos (pp s)

(** [create_meta_var s] creates a meta-variable thanks to a name.
    Note: The option -r can't change the name. *)
let create_meta_var : string -> p_meta_ident = fun s -> no_pos (Name s)

(** P_term *)

(** [p_TYPE] is the constant TYPE without position. *)
let p_TYPE : p_term = no_pos P_Type

(** [create_ident s] creates an identifier (just a name) without position.
    This identifier is not prefixed by "@", and can be changed by the option -r. *)
let create_ident : string -> p_term = fun s ->
  let tmp = create_p_qident ([], s) in
  no_pos (P_Iden(tmp, false))

let p_SORTK = create_ident _SORTK
let p_INJD  = create_ident _INJD
let p_INJ   = create_ident _INJ
let p_KSEQ  = create_ident _KSEQ
let p_DOTK  = create_ident _DOTK

let p_TRUE  = create_ident _TRUE
let p_FALSE = create_ident _FALSE

(** [p_WILD] is the constant "_" without position. *)
let p_WILD : p_term = no_pos P_Wild

(** [create_meta s] creates a meta-variable without argument and position.
    Note: The option -r can't change the name. *)
let create_meta : string -> p_term = fun s ->
  no_pos (P_Meta (create_meta_var s, None))

(** [create_pattern_var s] creates a variable of pattern without argument and
    position. Note: The option -r can change the name. *)
let create_pattern_var : string -> p_term = fun s ->
  let name = Some (create_p_ident s) in
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
     let typ = Some p_SORTK in
     let is_implicit = true in
     [ List.map unique_name s_l, typ, is_implicit ]

(** [create_p_symbol mod_l name param_l typ def] creates a p_symbol with:
    a list of modifiers [mod_l], a name [name], a list of parameters [param_l],
    an optional type [typ], an optional definition [def].
    Note: The option -r can change the name. *)
let create_p_symbol (mod_l : p_modifier list) (name : string) (param_l : p_params list)
  (typ  : p_term option) (def : p_term option) : p_symbol =
  let is_def = match def with None -> false | Some _ -> true in
  { p_sym_mod = mod_l
  ; p_sym_nam = create_p_ident name
  ; p_sym_arg = param_l
  ; p_sym_typ = typ
  ; p_sym_trm = def
  ; p_sym_prf = None
  ; p_sym_def = is_def}

(** [create_compute_command p] creates a command to compute the p_term [p]. *)
let create_compute_command : p_term -> p_command = fun p ->
  no_pos (P_query (no_pos (P_query_normalize (p, {strategy=NONE ; steps=None}))))

(** [create_builtin_command opt sym] creates a builtin command,
    i.e. builtin [opt] := [sym]. *)
let create_builtin_command : string -> path * string -> p_command = fun opt sym ->
  no_pos (P_builtin(opt, (create_p_qident sym)))

(* @TODO move? *)
module StrMap = Map.Make(String)
