(** Parser-level abstract syntax. *)

(*open! Lplib
open Lplib.Base
open Lplib.Extra
open Common
open Pos
open Core*)
open Pos

type path = string list

type qident = path * string

(** Pattern-matching strategy modifiers. *)
type match_strat =
  | Sequen
  (** Rules are processed sequentially: a rule can be applied only if the
      previous ones (in the order of declaration) cannot be. *)
  | Eager
  (** Any rule that filters a term can be applied (even if a rule defined
      earlier filters the term as well). This is the default. *)

(** Specify the visibility and usability of symbols outside their module. *)
type expo =
  | Public (** Visible and usable everywhere. *)
  | Protec (** Visible everywhere but usable in LHS arguments only. *)
  | Privat (** Not visible and thus not usable. *)

(** Symbol properties. *)
type prop =
  | Defin (** Definable. *)
  | Const (** Constant. *)
  | Injec (** Injective. *)
  | Commu (** Commutative. *)
  | Assoc of bool (** Associative left if [true], right if [false]. *)
  | AC of bool (** Associative and commutative. *)

(** The priority of an infix operator is a floating-point number. *)
type priority = float

type associativity =
  | Left
  | Right
  | Neither

(** Notations. *)
type notation =
  | Prefix of priority
  | Infix of associativity * priority
  | Zero
  | Succ
  | Quant

(** Type representing the different evaluation strategies. *)
type strategy =
  | WHNF (** Reduce to weak head-normal form. *)
  | HNF  (** Reduce to head-normal form. *)
  | SNF  (** Reduce to strong normal form. *)
  | NONE (** Do nothing. *)

(** Configuration for evaluation. *)
type config =
  { strategy : strategy   (** Evaluation strategy.          *)
  ; steps    : int option (** Max number of steps if given. *) }


(** Representation of a (located) identifier. *)
type p_ident = strloc

(** [notin id idopts] checks that [id] does not occur in [idopts]. *)
let check_notin : string -> p_ident option list -> unit = fun id ->
  let rec notin = function
    | [] -> ()
    | None :: idopts -> notin idopts
    | Some {elt=id';pos} :: idopts ->
        if id' = id then fatal pos "%s already used." id
        else notin idopts
  in notin

(** [are_distinct idopts] checks that the elements of [idopts] of the form
   [Some _] are pairwise distinct. *)
let rec check_distinct : p_ident option list -> unit = function
  | [] -> ()
  | None :: idopts -> check_distinct idopts
  | Some {elt=id;_} :: idopts -> check_notin id idopts; check_distinct idopts

(** Identifier of a metavariable. *)
type meta_ident = Name of string | Numb of int
type p_meta_ident = meta_ident loc

(** Representation of a module name. *)
type p_path = path loc

(** Representation of a possibly qualified (and located) identifier. *)
type p_qident = qident loc

(** Parser-level (located) term representation. *)
type p_term = p_term_aux loc
and p_term_aux =
  | P_Type (** TYPE constant. *)
  | P_Iden of p_qident * bool (** Identifier. The boolean indicates whether
                                 the identifier is prefixed by "@". *)
  | P_Wild (** Underscore. *)
  | P_Meta of p_meta_ident * p_term array option
    (** Meta-variable application. *)
  | P_Patt of p_ident option * p_term array option (** Pattern. *)
  | P_Appl of p_term * p_term (** Application. *)
  | P_Arro of p_term * p_term (** Arrow. *)
  | P_Abst of p_params list * p_term (** Abstraction. *)
  | P_Prod of p_params list * p_term (** Product. *)
  | P_LLet of p_ident * p_params list * p_term option * p_term * p_term
    (** Let. *)
  | P_NLit of int (** Natural number literal. *)
  | P_Wrap of p_term (** Term between parentheses. *)
  | P_Expl of p_term (** Term between curly brackets. *)

(** Parser-level representation of a function argument. The boolean is true if
    the argument is marked as implicit (i.e., between curly braces). *)
and p_params = p_ident option list * p_term option * bool

(** [nb_params ps] returns the number of parameters in a list of parameters
    [ps]. *)
let nb_params : p_params list -> int =
  List.fold_left (fun acc (ps,_,_) -> acc + List.length ps) 0

(** [get_impl_params_list l] gives the implicitness of [l]. *)
let rec get_impl_params_list : p_params list -> bool list = function
  | [] -> []
  | (params,_,impl)::params_list ->
      List.map (fun _ -> impl) params @ get_impl_params_list params_list

(** [get_impl_term t] gives the implicitness of [t]. *)
let rec get_impl_term : p_term -> bool list = fun t -> get_impl_term_aux t.elt
and get_impl_term_aux : p_term_aux -> bool list = fun t ->
  match t with
  | P_Prod([],t) -> get_impl_term t
  | P_Prod((ys,_,impl)::xs,t) ->
      List.map (fun _ -> impl) ys @ get_impl_term_aux (P_Prod(xs,t))
  | P_Arro(_,t)  -> false :: get_impl_term t
  | P_Wrap(t)    -> get_impl_term t
  | _            -> []

(** [p_get_args t] is {!val:LibTerm.get_args} on syntax-level terms. *)
let p_get_args : p_term -> p_term * p_term list = fun t ->
  let rec p_get_args t acc =
    match t.elt with
    | P_Appl(t, u) -> p_get_args t (u::acc)
    | _            -> t, acc
  in p_get_args t []

(** Parser-level rewriting rule representation. *)
type p_rule_aux = p_term * p_term
type p_rule = p_rule_aux loc

(** Parser-level inductive type representation. *)
type p_inductive_aux = p_ident * p_term * (p_ident * p_term) list
type p_inductive = p_inductive_aux loc

(** Rewrite patterns as in Coq/SSReflect. See "A Small Scale
    Reflection Extension for the Coq system", by Georges Gonthier,
    Assia Mahboubi and Enrico Tassi, INRIA Research Report 6455, 2016,
    @see <http://hal.inria.fr/inria-00258384>, section 8, p. 48. *)
type ('term, 'binder) rw_patt =
  | Rw_Term           of 'term
  | Rw_InTerm         of 'term
  | Rw_InIdInTerm     of 'binder
  | Rw_IdInTerm       of 'binder
  | Rw_TermInIdInTerm of 'term * 'binder
  | Rw_TermAsIdInTerm of 'term * 'binder

type p_rw_patt = (p_term, p_ident * p_term) rw_patt loc

(** Parser-level representation of an assertion. *)
type p_assertion =
  | P_assert_typing of p_term * p_term
  (** The given term should have the given type. *)
  | P_assert_conv   of p_term * p_term
  (** The two given terms should be convertible. *)

(** Parser-level representation of a query command. *)
type p_query_aux =
  | P_query_verbose of int
  (** Sets the verbosity level. *)
  | P_query_debug of bool * string
  (** Toggles logging functions described by string according to boolean. *)
  | P_query_flag of string * bool
  (** Sets the boolean flag registered under the given name (if any). *)
  | P_query_assert of bool * p_assertion
  (** Assertion (must fail if boolean is [true]). *)
  | P_query_infer of p_term * config
  (** Type inference command. *)
  | P_query_normalize of p_term * config
  (** Normalisation command. *)
  | P_query_prover of string
  (** Set the prover to use inside a proof. *)
  | P_query_prover_timeout of int
  (** Set the timeout of the prover (in seconds). *)
  | P_query_print of p_qident option
  (** Print information about a symbol or the current goals. *)
  | P_query_proofterm
  (** Print the current proof term (possibly containing open goals). *)

type p_query = p_query_aux loc

(** Parser-level representation of a proof tactic. *)
type p_tactic_aux =
  | P_tac_admit
  | P_tac_apply of p_term
  | P_tac_assume of p_ident option list
  | P_tac_fail
  | P_tac_focus of int
  | P_tac_generalize of p_ident
  | P_tac_have of p_ident * p_term
  | P_tac_induction
  | P_tac_query of p_query
  | P_tac_refine of p_term
  | P_tac_refl
  | P_tac_rewrite of bool * p_rw_patt option * p_term
  (* The boolean indicates if the equation is applied from left to right. *)
  | P_tac_simpl of p_qident option
  | P_tac_solve
  | P_tac_sym
  | P_tac_why3 of string option

type p_tactic = p_tactic_aux loc

(** Parser-level representation of a proof terminator. *)
type p_proof_end_aux =
  | P_proof_end
  (** The proof is done and fully checked. *)
  | P_proof_admitted
  (** Give up current state and admit the theorem. *)
  | P_proof_abort
  (** Abort the proof (theorem not admitted). *)

type p_proof_end = p_proof_end_aux loc

(** Parser-level representation of modifiers. *)
type p_modifier_aux =
  | P_mstrat of match_strat (** pattern matching strategy *)
  | P_expo of expo (** visibility of symbol outside its modules *)
  | P_prop of prop (** symbol properties: constant, definable, ... *)
  | P_opaq (** opacity *)

type p_modifier = p_modifier_aux loc

let is_prop {elt; _} = match elt with P_prop(_) -> true | _ -> false
let is_opaq {elt; _} = match elt with P_opaq -> true | _ -> false
let is_expo {elt; _} = match elt with P_expo(_) -> true | _ -> false
let is_mstrat {elt; _} = match elt with P_mstrat(_) -> true | _ -> false

(** Parser-level representation of symbol declarations. *)
type p_symbol =
  { p_sym_mod : p_modifier list (** modifiers *)
  ; p_sym_nam : p_ident (** symbol name *)
  ; p_sym_arg : p_params list (** arguments before ":" *)
  ; p_sym_typ : p_term option (** symbol type *)
  ; p_sym_trm : p_term option (** symbol definition *)
  ; p_sym_prf : (p_tactic list * p_proof_end) option (** proof script *)
  ; p_sym_def : bool (** is it a definition ? *) }

(** Parser-level representation of a single command. *)
type p_command_aux =
  | P_require  of bool * p_path list
    (* "require open" if the boolean is true *)
  | P_require_as of p_path * p_ident
  | P_open of p_path list
  | P_symbol of p_symbol
  | P_rules of p_rule list
  | P_inductive of p_modifier list * p_params list * p_inductive list
  | P_builtin of string * p_qident
  | P_notation of p_qident * notation
  | P_unif_rule of p_rule
  | P_query of p_query

(** Parser-level representation of a single (located) command. *)
type p_command = p_command_aux loc

(** Top level AST returned by the parser. *)
type ast = p_command Stream.t
