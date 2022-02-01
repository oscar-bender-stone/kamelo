(** This file describes the most general type that you can used for the output of a
    translator to Dedukti.

     More precisely, there are two kind of declarations: safe and unsafe one.
       * The following are safe:
          - Import a file
          - Comment
          - Builtin ?
          - Inductive
          - Logic statement: If you change the logic/extend the logic,
                             you use a logic statement.
              A. Symbol
              B. Definition
              C. Rewriting rule
          - Set option: If you change file configuration without changing the logic,
                        you setup your file thanks to a set option.
                        (= file management (some parameters))
          - Query: If you don't change anything (nor file parameters, neither the logic),
                   you use a query.
       * The following are unsafe:
          - Unification rule
          - Sequential

  Note: For "Require" and "Inductive" -> on étend pas vraiment la logique
        En effet, il est possible d'écrire la même logique en fusionnant les fichiers,
        alors qu'on ne peut pas remplacer "symbol" et "rule".
        La même explication concerne "inductive", pour l'instant. *)

open Term

(* TODO-list:
     - Forme des preuves
     - Builtin à la bonne place ?
     - Binder  à la bonne place ?
     - Assoc dans AC / ACU ?
*)

(** ********************* *)
(**  Logic statement      *)
(** ********************* *)

(** ***************** *)
(** A. Symbol         *) (* ~ axiom ? *)
(** ***************** *)

(** Parsing *)

type associativity = Left | Right | Non_assoc (* default *)
type precedence = int
type mixfix = Infix of associativity | Prefix | Postfix | Closed (* TODO Binder ? *)

(* (** Notations. *)
type notation =
  | Prefix of priority
  | Infix of Pratter.associativity * priority
  | Zero
  | Succ
  | Quant *)

(* BINDER ?
quantifier Allows to write `f x, t instead of f (λ x, t):

symbol ∀ {a} : (T a → Prop) → Prop;
notation ∀ quantifier;
compute λ p, ∀ (λ x:T a, p); // prints `∀ x, p
type λ p, `∀ x, p; // quantifiers can be written as such
type λ p, `f x, p; // works as well if f is any symbol
                                       *)

type parsing_rule = { mixfix : mixfix ; prec : precedence }

(** Visibility *)

type visibility =
  | Private   (** Not visible and thus not usable. *)
  | Public    (** Visible and usable everywhere.   *)
  | Protected (** Visible everywhere but usable in LHS arguments only. *)

(** Property *)

type algebra = Free | C | AC (* of bool ? TODO *) | ACU of term (* _ * bool ? TODO *) (** Symbols' algebra *)
type property = Static | Definable of algebra | Injective

(** Main type *)

type sym_identity =
  { parsing : parsing_rule  (** parsing rule *)
  ; visibility : visibility (** Specify the visibility and usability of symbols outside their module. *)
  ; prop : property         (** symbol properties: static, definable or injective *)
  (* ; strat : match_strat     (** pattern matching strategy *) *)
  ; name : iden             (** symbol name *) }

type param =
  | Impl of iden * term option
  | Expl of iden * term option

(** arguments before ":" + type after ":" *)
type type_decl = param list * term

type sym_decl = { sym : sym_identity
                ; typ : type_decl }

(** ***************** *)
(** B. Definition     *) (* ~ notation ? *)
(** ***************** *)

(* TODO
let rw_patt : p_rw_patt pp = fun ppf p ->
  match p.elt with
  | Rw_Term(t)               -> term ppf t
  | Rw_InTerm(t)             -> out ppf "in %a" term t
  | Rw_InIdInTerm(x,t)       -> out ppf "in %a in %a" ident x term t
  | Rw_IdInTerm(x,t)         -> out ppf "%a in %a" ident x term t
  | Rw_TermInIdInTerm(u,(x,t)) ->
      out ppf "%a in %a in %a" term u ident x term t
  | Rw_TermAsIdInTerm(u,(x,t)) ->
      out ppf "%a as %a in %a" term u ident x term t
      *)
type tactic =
  | Admit
  | Refine of term
  | Assume of iden option list
  | Apply of term
  | Generalize of iden
  | Have of iden * term
  | Simpl of iden option
  | Rewrite of bool (* TODO * p_rw_patt option *) * term
  (* The boolean indicates if the equation is applied from left to right. *)
  | Reflexivity
  | Symmetry
  | Induction
  | Solve
  | Why3 of string option
  | Focus of int (* specific one - where ? *)
  | Fail  (* specific one *)
 (* | Query ? *)

(* TODO
(** Parser-level representation of a proof. *)
     type p_subproof = p_proofstep list
     and p_proofstep = Tactic of p_tactic * p_subproof list

     type p_proof = p_subproof list
 *)
type proof = tactic list (* TODO *)


type proof_ending =
  | End      (** The proof is done and fully checked.         *)
  | Admitted (** Give up current state and admit the theorem. *)
  | Abort    (** Abort the proof (theorem not admitted).      *)


type def_body =
  | LambdaTerm of term                      (** Lambda term  *)
  | Script of (proof * proof_ending) option (** Proof script *)

type def_decl =
  { sym : sym_identity
  ; typ : type_decl option
  ; def : def_body
  ; opacity : bool }

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

type rule = pattern * term
type rule_decl = rule list

type logic_statement =
  | Symbol     of sym_decl
  | Definition of def_decl
  | Rule       of rule_decl

(** ********************* *)
(**  Set option           *)
(** ********************* *)

type printing_option  = Implicit | Context | Domain | Meta_type | Meta_arg
type rewriting_option = Eta_equalify
type flag_opt =
  | Print_opt   of printing_option  (** Some flags mainly for modifying the printing behavior. *)
  | Rewrite_opt of rewriting_option (** Only the flag "Eta_equalify" changes the behavior of
                                        the rewrite engine by reducing η-redexes. *)

type prover = Alt_Ergo (* default *) | EProver (* TODO list ? *)

type set_option =
  | Debug of bool * string       (** Toggles logging functions described by string
                                     according to boolean. *)
  | Verbosity of int             (** Sets the verbosity level. *)
  | Flag of flag_opt * bool      (** Sets the boolean flag registered under
                                     the given name (if any). *)
  | Prover of
      prover option * int option (** Set the prover to use inside a proof,
                                     and the timeout of the prover (in seconds). *)
  (* | Quantifier of ? *)

(** ********************* *)
(**  Query                *)
(** ********************* *)

type strat  = SNF | WHNF (* TODO ? CBV ? CBSV ? *)
type config = int option * strat option
type op     = Conv of bool | HasType of bool (* | NConv | NType *)
type opt    = ProofTerm (* default *) | Goal (* snif String *)

(*
 | P_query_print of p_qident option
  (** Print information about a symbol or the current goals. *)
  | P_query_proofterm
  (** Print the current proof term (possibly containing open goals). *)
 *)
(* Finally, the queries are only: print, proofterm, type, compute, assert, assertnot
   and they just give an answer to a question, without changing anything. *)
type query =
  | Eval   of config * term            (** Evaluation *) (* + affichage d'un terme *)
  | Infer  of config * term            (** Type inference command *)
  | Check  of op * term * term         (** Checking *)
  | Assert of op * term * term         (** Assertion *)
  | DTree                              (** Affichage d'un arbre de décision     *)
  | Print  of opt option * term option (** Affichage d'un terme, d'un but ou
                                           d'un terme de preuve                 *)
  | SPrint of string                   (** Affichage d'une chaine de caractères *)
(* | Name (_, _) -> () OBSELETE *)

(** ********************* *)
(**  (Un)safe command     *)
(** ********************* *)

type name = string
type path = string (* TODO *)
type filename = path * iden

type import_decl =
  | Require    of bool * path list (* "require open" if the boolean is true *)
  | Require_as of path * iden
  | Open       of path list
(** require: Informs the type-checker that the current module depends on
             some other module, which must hence be compiled.
    open: Puts into scope the symbols of the previously required module given in argument.
          It can also be combined with the require command. *)

type builtin_opt =
  | Zero   (*   "0"    *)
  | Succ   (*   "+1"   *)
  | Set    (*   "T"    *)
  | Prop   (*   "P"    *)
  | Eq     (*   "eq"   *)
  | Refl   (*  "refl"  *)
  | Eq_ind (* "eq_ind" *)
  | Top    (*  "top"   *)
  | Bot    (*  "bot"   *)
  | Not    (*  "not"   *)
  | Or     (*   "or"   *)
  | And    (*  "and"   *)
  | Imp    (*  "imp"   *)

type induc_decl =
  { name : iden
  ; typ  : type_decl
  ; constructor : (iden * type_decl) list }

type command =
  | Import  of import_decl
  | Comment of string
  | Builtin of builtin_opt * iden (* TODO bonne place ? *)
  | Inductive  of visibility * param list * induc_decl list
  | Logic_stmt of logic_statement
  | Set_option of set_option
  | Query of query


type unsafe_command =
  | Unif_rule  of rule
  | Sequential of sym_decl
     (* Property that modifies the pattern matching algorithm.
        By default, the order of rule declarations is not taken into account.
        This property tells Lambdapi to apply rules defining a sequential symbol in the order they have been declared
        (note that the order of the rules may depend on the order of the require commands).
        WARNING: using this modifier can break important properties. *)
  | Safe_command of command
