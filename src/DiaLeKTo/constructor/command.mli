open Type.Command
open Type.Term

(** ********************* *)
(**  Logic statement      *)
(** ********************* *)

(** ***************** *)
(** A. Symbol         *)
(** ***************** *)

(** Parsing *)
val v_default_mixfix  : mixfix
val v_infix_left      : mixfix
val v_infix_right     : mixfix
val v_infix_non_assoc : mixfix
val v_prefix          : mixfix
val v_postfix         : mixfix
val v_closed          : mixfix

val v_default_prec : precedence
val cr_prec : int -> precedence

val cr_parsing_rule : mixfix -> precedence -> parsing_rule

(** Visibility *)
val v_private   : visibility
val v_public    : visibility
val v_protected : visibility

(** Property *)
val v_static         : property
val v_definable_free : property
val v_definable_C    : property
val v_definable_AC   : property
val v_definable_ACU  : term -> property
val v_injective      : property

(** Type parameter *)
val cr_impl_param : iden -> term option -> param
val cr_expl_param : iden -> term option -> param

val cr_type_decl : param list -> term -> type_decl

(** Symbol declaration *)
val cr_symbol : parsing_rule -> visibility -> property -> iden -> type_decl -> command (* TODO sym_decl ? *)

(** [cr_symbol_default] creates a symbol with:
      - its parsing rule = v_prefix with a precedence of 42
      - its visibility   = v_public *)
val cr_symbol_default : property -> iden -> type_decl -> command (* TODO sym_decl  ? *)

(** ***************** *)
(** B. Definition     *)
(** ***************** *)

(** Lambda-term *)
val cr_lambda_term : term -> def_body

(** Proof script *)
(*
let v_admit       : tactic
let v_refine      : term -> tactic
let v_assume      : 'a list -> tactic
let v_apply       : term -> tactic
let v_generalize  : 'a -> tactic
let v_have        : 'a -> term -> tactic
let v_simpl       : 'a -> tactic
let v_rewrite     : -> -> -> tactic
let v_reflexivity : tactic
let v_symmetry    : tacitc
let v_induction   : tactic
let v_solve       : tactic
let v_why3        : int -> prover -> tactic
let v_focus       : int -> tactic
let v_fail        : tactic *)

val v_qed   : proof_ending
val v_admit : proof_ending
val v_abort : proof_ending

(** [cr_script p e] creates a proof script. *)
val cr_script : proof -> proof_ending -> def_body

(** Definition declaration *)

(** [cr_definition pr v p n td def opaq] creates a definition declaration with:
      - its parsing rule [pr]
      - its visibility [v]
      - its property [p]
      - its name [n]
      - its type [td]
      - its body, definition.
    Note: [opaq] is "true" if the definition is opaque. *)
val cr_definition : parsing_rule -> visibility -> property -> iden -> type_decl -> def_body -> bool -> command (* TODO def_decl ? *)

(** [cr_def_no_type pr v p n def opaq] creates a definition declaration
    as [cr_definition] but without specifying the type. *)
val cr_def_no_type : parsing_rule -> visibility -> property -> iden -> def_body -> bool -> command (* TODO def_decl ? *)

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

(* TODO *)

(** ********************* *)
(**  Set option           *)
(** ********************* *)

(** Flag option *)
val v_implicit     : flag_opt
val v_context      : flag_opt
val v_domain       : flag_opt
val v_meta_typ     : flag_opt
val v_meta_arg     : flag_opt
val v_eta_equalify : flag_opt

(** Prover *)
val v_Alt_Ergo : prover
val v_EProver  : prover

(* TODO
(** [cr_prover_opt ]  *)
let cr_prover_opt : ~p:Alt_Ergo -> ~n:2 -> set_option = Prover(p, n)
 *)

(** ********************* *)
(**  Query                *)
(** ********************* *)

val v_SNF  : strat
val v_WHNF : strat

type nb_step = int option
type config = nb_step * strat option

val v_nb_step : int option -> nb_step
val v_config : nb_step -> strat option -> config

val v_conversion     : op
val v_conversion_not : op
val v_has_type       : op
val v_has_type_not   : op

val v_proofterm : opt
val v_goal      : opt

val cr_eval   : config -> term -> command
val cr_infer  : config -> term -> command
val cr_check  : op -> term -> term -> command
val cr_assert : op -> term -> term -> command

val cr_pp_deci_tree  : command
val cr_pp_term       : term -> command
val cr_pp_goal       : command
val cr_pp_proof_term : command
val cr_pp_message    : string -> command

(** ********************* *)
(** Safe command          *)
(** ********************* *)

(** Note that:
      - logic statement: symbol, definition, rewriting rule;
      - set option: debug, verbose, flag, prover;
      - query: eval, infer, check, assert, dtree, print;
    are also safe commands. *)

(** Importation command *)

(* type command =
  | Import  of import_decl

type import_decl =
  | Require    of bool * path list (* "require open" if the boolean is true *)
  | Require_as of path * iden
  | Open       of path list *)

(** [cr_include f] includes the file [f]. *)
val cr_include : filename -> command

(* let cr_import  : bool -> path -> alias -> command = fun i TODO *)

(** Comment command *)
val cr_comment : string -> command

(** Builtin command *)
val v_Zero   : builtin_opt
val v_Succ   : builtin_opt
val v_Set    : builtin_opt
val v_Prop   : builtin_opt
val v_Eq     : builtin_opt
val v_Refl   : builtin_opt
val v_Eq_ind : builtin_opt
val v_Top    : builtin_opt
val v_Bot    : builtin_opt
val v_Not    : builtin_opt
val v_Or     : builtin_opt
val v_And    : builtin_opt
val v_Imp    : builtin_opt

(** [cr_builtin b_opt n] links a pre-defined symbol [b_opt] with user-symbol [n]. *)
val cr_builtin : builtin_opt -> name -> command

(** Inductive command *)

(** You can see a constructor as a symbol with:
      - its parsing rule = v_prefix with a precedence of 42
      - its visibility   = v_public
      - its property     = v_constant ? v_injective ?
    So, you need to give a name and a type declaration. *)
type constructor = iden * type_decl

(** [cr_simple_inductive v n td c_l] creates a non-mutual inductive type with
    the visibility [v], the name [n], the type [td], the constructor(s) [c_l]. *)
val cr_simple_inductive : visibility -> iden -> type_decl -> constructor list -> induc_decl (* TODO command ? *)

(** [cr_mutual_inductive v p_l induc_l] creates a mutual inductive type,
    i.e. a combinaison of simple inductive declarations [induc_l],
    with a common visibility [v] andcommon type parameters [p_l].

    Example of a mutual inductive type in Coq:

      Inductive T (a:Set) : Type :=
       | node : a -> F a -> T a
      with F (a:Set) : Type :=
       | nilF : F a
       | consF : T a -> F a -> F a.

    but as Coq said "Parameters should be syntactically the same for each inductive type.".
    Also, in Dedukti, the previous inductive type is written:

      (a:Set) inductive T : TYPE ≔
       | node : τ a → F a → T a
      with F : TYPE ≔
       | nilF : F a
       | consF : T a → F a → F a;

   Note: The visibility of each simple inductive declaration is replaced by the common one.
   Same for type parameters ? *)
val cr_mutual_inductive : visibility -> param list -> induc_decl list -> command

(** ********************* *)
(** Unsafe command        *)
(** ********************* *)

val c_unif_rule      : rule         -> unsafe_command
val c_sequential_sym : sym_decl     -> unsafe_command
val to_unsafe        : command list -> unsafe_command list
