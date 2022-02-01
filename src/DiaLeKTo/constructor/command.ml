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

open Type.Command
open Type.Term

(*
let v_left     : associativity = Left
let v_right    : associativity = Right
let v_non_asso : associativity = Non_asso

type precedence = int
let v_prec : int -> precedence = fun i -> i

type mixfix = Infix of associativity | Prefix | Postfix | Closed (* TODO Binder ? *)

let v_infix   : associativity -> mixfix = fun a -> Infix a
 *)
   
(** ********************* *)   
(**  Logic statement      *)
(** ********************* *)

(** ***************** *)
(** A. Symbol         *)
(** ***************** *)

(** Parsing *)
let v_default_mixfix  : mixfix = Prefix
let v_infix_left      : mixfix = Infix Left
let v_infix_right     : mixfix = Infix Right
let v_infix_non_assoc : mixfix = Infix Non_assoc
let v_prefix          : mixfix = Prefix
let v_postfix         : mixfix = Postfix
let v_closed          : mixfix = Closed

let v_default_prec : precedence = 42
let cr_prec : int -> precedence = fun i -> i

let cr_parsing_rule : mixfix -> precedence -> parsing_rule =
  fun m p -> { mixfix = m ; prec = p }

(** Visibility *)
let v_private   : visibility = Private
let v_public    : visibility = Public
let v_protected : visibility = Protected

(** Property *)
let v_static : property = Static
let v_definable_free : property = Definable Free
let v_definable_C    : property = Definable C
let v_definable_AC   : property = Definable AC
let v_definable_ACU  : term -> property = fun t -> Definable (ACU t)
let v_injective : property = Injective

(** Type parameter *)
let cr_impl_param : iden -> term option -> param =
  fun n t -> Impl(n, t)
let cr_expl_param : iden -> term option -> param =
  fun n t -> Expl(n, t)

let cr_type_decl : param list -> term -> type_decl =
  fun param_l t -> param_l, t

(** Symbol declaration *)

(* not in .mli *)
let cr_sym_identity : parsing_rule -> visibility -> property -> iden -> sym_identity =
  fun pr v p n -> { parsing = pr ; visibility = v ; prop = p ; name = n }

(* not in .mli *)
let crs_symbol : parsing_rule -> visibility -> property -> iden -> type_decl -> sym_decl =
  fun pr v p n td -> { sym = cr_sym_identity pr v p n ; typ = td }

(* not in .mli *)
let crs_symbol_default : property -> iden -> type_decl -> sym_decl =
  fun p n td -> { sym = cr_sym_identity (cr_parsing_rule v_prefix (cr_prec 42)) v_public p n ; typ = td }



let cr_symbol : parsing_rule -> visibility -> property -> iden -> type_decl -> command =
  fun pr v p n td -> Logic_stmt (Symbol (crs_symbol pr v p n td))

let cr_symbol_default : property -> iden -> type_decl -> command =
  fun p n td -> Logic_stmt (Symbol (crs_symbol_default p n td))

(** ***************** *)
(** B. Definition     *)
(** ***************** *)

(** Lambda-term *)
let cr_lambda_term : term -> def_body = fun t -> LambdaTerm t

(** Proof script *)
(*
let v_admit       : tactic = Admit
let v_refine      : term -> tactic = fun t -> Refine t
let v_assume      : -> tactic = Assume (* TODO *)
let v_apply       : term -> tactic = fun t -> Apply t
let v_generalize  : -> tactic = fun -> Generalize (* TODO *)
let v_have        : -> term -> tactic = fun  t -> Have( , t) (* TODO *)
let v_simpl       : -> tactic = fun -> Simpl (* TODO *)
let v_rewrite     : -> -> -> tactic = fun -> Rewrite (* TODO *)
let v_reflexivity : tactic = Reflexivity
let v_symmetry    : tacitc = Symmetry
let v_induction   : tactic = Induction
let v_solve       : tactic = Solve
let v_why3        : -> tactic = fun p -> Why3 p (* TODO *)
let v_focus       : tactic = Focus
let v_fail        : tactic = Fail
                             *)
let v_qed   : proof_ending = End
let v_admit : proof_ending = Admitted
let v_abort : proof_ending = Abort


let cr_script : proof -> proof_ending -> def_body =
  fun p e -> Script (Some(p, e))

(** Definition declaration *)
let crd_definition : parsing_rule -> visibility -> property -> iden -> type_decl -> def_body -> bool -> def_decl =
  fun pr v p n td def opaq -> { sym = cr_sym_identity pr v p n ; typ = Some td ; def = def ; opacity = opaq }

let crd_def_no_type : parsing_rule -> visibility -> property -> iden -> def_body -> bool -> def_decl =
  fun pr v p n def opaq -> { sym = cr_sym_identity pr v p n ; typ = None ; def = def ; opacity = opaq }


let cr_definition : parsing_rule -> visibility -> property -> iden -> type_decl -> def_body -> bool -> command =
  fun pr v p n td def opaq ->
  Logic_stmt (Definition (crd_definition pr v p n td def opaq))

let cr_def_no_type : parsing_rule -> visibility -> property -> iden -> def_body -> bool -> command =
  fun pr v p n def opaq ->
  Logic_stmt (Definition (crd_def_no_type pr v p n def opaq))

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

(* TODO let cr_rule : rule_decl -> command = fun r -> Logic_stmt (Rule r) *)

(** ********************* *)
(**  Set option           *)
(** ********************* *)

let v_implicit : flag_opt = Print_opt Implicit
let v_context  : flag_opt = Print_opt Context
let v_domain   : flag_opt = Print_opt Domain
let v_meta_typ : flag_opt = Print_opt Meta_type
let v_meta_arg : flag_opt = Print_opt Meta_arg
let v_eta_equalify : flag_opt = Rewrite_opt Eta_equalify

(** Flag option *)
(*
type set_option =
  | Debug of bool * string   (** Toggles logging functions described by string according to boolean. *)
  | Verbosity of int         (** Sets the verbosity level. *)
  | Flag of flag_opt * bool  (** Sets the boolean flag registered under the given name (if any). *)
  | Prover of prover * int   (** Set the prover to use inside a proof, and the timeout of the prover (in seconds). *)
  (* | Quantifier of ? *)
              *)

(** Prover *)
let v_Alt_Ergo : prover = Alt_Ergo (* default *)
let v_EProver  : prover = EProver

(* TODO
(** [cr_prover_opt ]  *)
let cr_prover_opt : ~p:Alt_Ergo -> ~n:2 -> set_option = Prover(p, n)
 *)

(* TODO let cr_set_option : set_option      -> command = fun s -> Set_option s *)

(** ********************* *)
(**  Query                *)
(** ********************* *)

let v_SNF  : strat = SNF
let v_WHNF : strat = WHNF

type nb_step = int option
type config = nb_step * strat option

let v_nb_step : int option -> nb_step = fun i -> i
let v_config : nb_step -> strat option -> config = fun nb_s strat -> nb_s, strat

let v_conversion     : op = Conv true
let v_conversion_not : op = Conv false
let v_has_type       : op = HasType true
let v_has_type_not   : op = HasType false

let v_proofterm : opt = ProofTerm
let v_goal      : opt = Goal

(*
(** Representation of a (located) identifier. *)
type p_ident = strloc

(** Representation of a possibly qualified identifier. *)
type qident = Path.t * string
              *)

let crq_eval   : config -> term -> query = fun c t -> Eval(c, t) (* TODO *)
let crq_infer  : config -> term -> query = fun c t -> Infer(c, t)
let crq_check  : op -> term -> term -> query = fun o t1 t2 -> Check(o, t1, t2)
let crq_assert : op -> term -> term -> query = fun o t1 t2 -> Assert(o, t1, t2)

let crq_pp_deci_tree  : query = DTree (* TODO *)
let crq_pp_term       : term -> query = fun t -> Print(None, Some t)
let crq_pp_goal       : query = Print(Some v_goal,      None)
let crq_pp_proof_term : query = Print(Some v_proofterm, None)
let crq_pp_message    : string -> query = fun s -> SPrint s

(* let from_query : query -> command = fun q -> Query q TODO mieux ? *)

let cr_eval   : config -> term -> command =
  fun c t -> Query (crq_eval c t)
let cr_infer  : config -> term -> command =
  fun c t -> Query (crq_infer c t)
let cr_check  : op -> term -> term -> command =
  fun o t1 t2 -> Query (crq_check o t1 t2)
let cr_assert : op -> term -> term -> command =
  fun o t1 t2 -> Query (crq_assert o t1 t2)

let cr_pp_deci_tree  : command = Query crq_pp_deci_tree
let cr_pp_term       : term -> command =
  fun t -> Query (crq_pp_term t)
let cr_pp_goal       : command = Query crq_pp_goal
let cr_pp_proof_term : command = Query crq_pp_proof_term
let cr_pp_message    : string -> command =
  fun s -> Query (crq_pp_message s)

(** ********************* *)
(** Safe command          *)
(** ********************* *)

(** Importation command *)
let cr_include : filename -> command = fun (_,n) -> Comment n (* TODO fix *)

(* let cr_import  : bool -> path -> alias -> command = fun i TODO *)

(** Comment command *)
let cr_comment : string -> command = fun s -> Comment s

(** Builtin command *)
let v_Zero   : builtin_opt = Zero   (*   "0"    *)
let v_Succ   : builtin_opt = Succ   (*   "+1"   *)
let v_Set    : builtin_opt = Set    (*   "T"    *)
let v_Prop   : builtin_opt = Prop   (*   "P"    *)
let v_Eq     : builtin_opt = Eq     (*   "eq"   *)
let v_Refl   : builtin_opt = Refl   (*  "refl"  *)
let v_Eq_ind : builtin_opt = Eq_ind (* "eq_ind" *)
let v_Top    : builtin_opt = Top    (*  "top"   *)
let v_Bot    : builtin_opt = Bot    (*  "bot"   *)
let v_Not    : builtin_opt = Not    (*  "not"   *)
let v_Or     : builtin_opt = Or     (*   "or"   *)
let v_And    : builtin_opt = And    (*  "and"   *)
let v_Imp    : builtin_opt = Imp    (*  "imp"   *)

let cr_builtin : builtin_opt -> name -> command = fun b_opt n -> Builtin(b_opt, n)

(** Inductive command *)

(** You can see a constructor as a symbol with:
      - its parsing rule = v_prefix with a precedence of 42
      - its visibility   = v_public
      - its property     = v_constant ? v_injective ?
    So, you need to give a name and a type declaration. *)
type constructor = iden * type_decl

let cr_simple_inductive : visibility -> iden -> type_decl -> constructor list -> induc_decl = fun _ n td c_l ->
  { name = n ; typ = td ; constructor = c_l }

let cr_mutual_inductive : visibility -> param list -> induc_decl list -> command =
  fun v p_l i_l -> Inductive(v, p_l, i_l) (* TODO *)

(** ********************* *)
(** Unsafe command        *)
(** ********************* *)

let c_unif_rule      : rule         -> unsafe_command = fun r -> Unif_rule r
let c_sequential_sym : sym_decl     -> unsafe_command = fun sym_d -> Sequential sym_d
let to_unsafe        : command list -> unsafe_command list = fun c_l -> List.map (fun x -> Safe_command x) c_l
