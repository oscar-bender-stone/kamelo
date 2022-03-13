(** Pretty-printing the parser-level AST.

    This module defines functions that allow printing elements of syntax found
    in the parser-level abstract syntax. This is used, for example, to print a
    file in the Lambdapi syntax, given the AST obtained when parsing a file in
    the legacy (Dedukti) syntax. *)

open Pos
open Syntax
open Format

let out = Format.fprintf

let pp_assoc : associativity pp = fun ppf assoc ->
  match assoc with
  | Neither -> ()
  | Left -> out ppf " left associative"
  | Right -> out ppf " right associative"

let pp_notation : notation pp = fun ppf notation ->
  match notation with
  | Prefix(p) -> out ppf "prefix %f" p
  | Infix(a,p) -> out ppf "infix%a %f" pp_assoc a p
  | Zero -> out ppf "builtin \"0\""
  | Succ -> out ppf "builtin \"+1\""
  | Quant -> out ppf "quantifier"



let pp_prop : prop pp = fun ppf p ->
  match p with
  | AC true -> out ppf "left associative commutative "
  | AC false -> out ppf "associative commutative "
  | Assoc true -> out ppf "left associative "
  | Assoc false -> out ppf "associative "
  | Const -> out ppf "constant "
  | Commu -> out ppf "commutative "
  | Defin -> ()
  | Injec -> out ppf "injective "

let pp_expo : expo pp = fun ppf e ->
  match e with
  | Privat -> out ppf "private "
  | Protec -> out ppf "protected "
  | Public -> ()

let pp_match_strat : match_strat pp = fun ppf s ->
  match s with
  | Eager -> ()
  | Sequen -> out ppf "sequential "

(* ends with a space *)
let modifier : p_modifier pp = fun ppf {elt; _} ->
  match elt with
  | P_expo(e)   -> pp_expo ppf e
  | P_mstrat(s) -> pp_match_strat ppf s
  | P_prop(p)   -> pp_prop ppf p
  | P_opaq      -> out ppf "opaque "

let pp_sep : string -> unit pp = fun s ff () -> Format.pp_print_string ff s

let list_pp : 'a pp -> string -> 'a list pp = fun pp_elt sep ->
  Format.pp_print_list ~pp_sep:(pp_sep sep) pp_elt

(* ends with a space if the list is not empty *)
let modifiers : p_modifier list pp = list_pp modifier ""

(** check whether identifiers are Lambdapi keywords. *)
let check_keywords = ref false

(** [mem_sorted cmp x l] tells whether [x] is in [l] assuming that [l] is
   sorted wrt [cmp]. *)
let mem_sorted : ('a -> 'a -> int) -> 'a -> 'a list -> bool = fun cmp x ->
  let rec mem_sorted l =
    match l with
    | [] -> false
    | y :: l ->
      match cmp x y with 0 -> true | n when n > 0 -> mem_sorted l | _ -> false
  in mem_sorted

let is_keyword : string -> bool =
  let kws =
    List.sort String.compare
      [ "abort"
      ; "admit"
      ; "admitted"
      ; "apply"
      ; "as"
      ; "assert"
      ; "assertnot"
      ; "associative"
      ; "assume"
      ; "begin"
      ; "builtin"
      ; "commutative"
      ; "compute"
      ; "constant"
      ; "debug"
      ; "end"
      ; "fail"
      ; "flag"
      ; "focus"
      ; "generalize"
      ; "have"
      ; "in"
      ; "induction"
      ; "inductive"
      ; "infix"
      ; "injective"
      ; "left"
      ; "let"
      ; "off"
      ; "on"
      ; "opaque"
      ; "open"
      ; "prefix"
      ; "print"
      ; "private"
      ; "proofterm"
      ; "protected"
      ; "prover"
      ; "prover_timeout"
      ; "quantifier"
      ; "refine"
      ; "reflexivity"
      ; "require"
      ; "rewrite"
      ; "right"
      ; "rule"
      ; "sequential"
      ; "set"
      ; "simplify"
      ; "solve"
      ; "symbol"
      ; "symmetry"
      ; "type"
      ; "TYPE"
      ; "unif_rule"
      ; "verbose"
      ; "why3"
      ; "with" ]
  in
  fun s ->
  (* NOTE this function may be optimised using a map, a hashtable, or using
     [match%sedlex]. *)
    mem_sorted String.compare s kws

let pp_uid = Format.pp_print_string
let pp_path : path pp = list_pp pp_uid "."

let raw_ident : popt -> string pp = fun pos ppf s ->
  if !check_keywords && is_keyword s then
    fatal pos "%s is a Lambdapi keyword." s
  else pp_uid ppf s

let pp_p_ident : p_ident pp = fun ppf {elt=s; pos} -> raw_ident pos ppf s

let pp_p_meta_ident : p_meta_ident pp = fun ppf {elt; pos} ->
  match elt with
  | Name s -> raw_ident pos ppf s
  | Numb i -> out ppf "%d" i

let pp_p_param_id : p_ident option pp = fun ppf idopt ->
  match idopt with
  | Some(id) -> out ppf "%a" pp_p_ident id
  | None     -> out ppf "_"

let pp_p_param_ids : p_ident option list pp = list_pp pp_p_param_id " "

let raw_path : popt -> path pp = fun pos -> list_pp (raw_ident pos) "."
let pp_p_path : p_path pp = fun ppf {elt=p;pos} -> raw_path pos ppf p

let pp_p_qident : p_qident pp = fun ppf {elt=(mp,s); pos} ->
  match mp with
  | [] -> raw_ident pos ppf s
  | _::_ -> out ppf "%a.%a" (raw_path pos) mp (raw_ident pos) s


(** The possible priority levels are [`Func] (top level, including abstraction
   and product), [`Appl] (application) and [`Atom] (smallest priority). *)
type priority = [`Func | `Appl | `Atom]

let array_pp : 'a pp -> string -> 'a array pp =
 fun pp_elt sep oc a ->
  let n = Array.length a in
  if n > 0 then pp_elt oc (Array.get a 0);
  for i = 1 to n - 1 do
    Format.fprintf oc "%s%a" sep pp_elt (Array.get a i)
  done

let rec pp_p_term : p_term pp = fun ppf t ->
  let empty_context = ref true in
  let rec atom ppf t = pp `Atom ppf t
  and appl ppf t = pp `Appl ppf t
  and func ppf t = pp `Func ppf t
  and pp priority ppf t =
    let env ppf ts =
      match ts with
      | None -> ()
      | Some [||] when !empty_context -> ()
      | Some ts -> out ppf "[%a]"(array_pp func "; ") ts
    in
    match (t.elt, priority) with
    | (P_Type              , _    ) -> out ppf "TYPE"
    | (P_Iden(qid,false)   , _    ) -> out ppf "%a" pp_p_qident qid
    | (P_Iden(qid,true )   , _    ) -> out ppf "@%a" pp_p_qident qid
    | (P_Wild              , _    ) -> out ppf "_"
    | (P_Meta(mid,ts)      , _    ) -> out ppf "?%a%a" pp_p_meta_ident mid env ts
    | (P_Patt(None   ,ts)  , _    ) -> out ppf "$_%a" env ts
    | (P_Patt(Some(x),ts)  , _    ) -> out ppf "$%a%a" pp_p_ident x env ts
    | (P_Appl(t,u)         , `Appl)
    | (P_Appl(t,u)         , `Func) -> out ppf "%a %a" appl t atom u
    | (P_Arro(a,b)         , `Func) -> out ppf "%a → %a" appl a func b
    | (P_Abst(xs,t)        , `Func) ->
        out ppf "λ%a, " pp_p_params_list xs;
        let fn (ids,_,_) = List.for_all ((=) None) ids in
        let ec = !empty_context in
        empty_context := ec && List.for_all fn xs;
        func ppf t;
        empty_context := ec
    | (P_Prod(xs,b)        , `Func) ->
        out ppf "Π%a, %a" pp_p_params_list xs func b
    | (P_LLet(x,xs,a,t,u)  , `Func) ->
        out ppf "let %a%a%a ≔ %a in %a"
          pp_p_ident x pp_p_params_list xs pp_p_typ a func t func u
    | (P_NLit(i)           , _    ) -> out ppf "%i" i
    (* We print minimal parentheses, and ignore the [Wrap] constructor. *)
    | (P_Wrap(t)           , _    ) -> pp priority ppf t
    | (P_Expl(t)           , _    ) -> out ppf "[%a]" func t
    | (_                   , _    ) -> out ppf "(%a)" func t
  in
  let rec toplevel ppf t =
    match t.elt with
    | P_Abst(xs,t) -> out ppf "λ%a, %a" pp_p_params_list xs toplevel t
    | P_Prod(xs,b) -> out ppf "Π%a, %a" pp_p_params_list xs toplevel b
    | P_Arro(a,b) -> out ppf "%a → %a" appl a toplevel b
    | P_LLet(x,xs,a,t,u) ->
        out ppf "let %a%a%a ≔ %a in %a"
          pp_p_ident x pp_p_params_list xs pp_p_typ a toplevel t toplevel u
    | _ -> func ppf t
  in
  toplevel ppf t

and pp_p_params : p_params pp = fun ppf (ids,ao,b) ->
  match (ao,b) with
  | (None   , false) -> out ppf "%a" pp_p_param_ids ids
  | (None   , true ) -> out ppf "{%a}" pp_p_param_ids ids
  | (Some(a), false) -> out ppf "(%a : %a)" pp_p_param_ids ids pp_p_term a
  | (Some(a), true ) -> out ppf "{%a : %a}" pp_p_param_ids ids pp_p_term a

(* starts with a space if the list is not empty *)
and pp_p_params_list : p_params list pp = fun ppf ->
  List.iter (out ppf " %a" pp_p_params)

(* starts with a space if <> None *)
and pp_p_typ : p_term option pp = fun ppf t ->
  Option.iter (out ppf " : %a" pp_p_term) t

let pp_p_rule : string -> p_rule pp = fun kw ppf {elt=(l,r);_} ->
  out ppf "%s %a ↪ %a" kw pp_p_term l pp_p_term r

let pp_p_inductive : string -> p_inductive pp =
  let cons ppf (id,a) = out ppf "\n| %a : %a" pp_p_ident id pp_p_term a in
  fun kw ppf {elt=(id,a,cs);_} ->
  out ppf "%s %a : %a ≔%a" kw pp_p_ident id pp_p_term a (list_pp cons "") cs

let pp_equiv : (p_term * p_term) pp = fun ppf (l,r) ->
  out ppf "%a ≡ %a" pp_p_term l pp_p_term r
(*
(** [unpack eqs] transforms a p_term of the form [LpLexer.cons
   (LpLexer.equiv t u) (LpLexer.cons (LpLexer.equiv v w) ...)]  into a
   list [[(t,u); (v,w); ...]]. See unif_rule.ml. *)
let rec unpack : p_term -> (p_term * p_term) list = fun eqs ->
  let is (p,s) id = p = Unif_rule.path && s = id.Term.sym_name in
  match Syntax.p_get_args eqs with
  | ({elt=P_Iden({elt;_},_); _}, [v; w]) ->
      if is elt Unif_rule.cons then
        match Syntax.p_get_args v with
        | ({elt=P_Iden({elt;_},_); _}, [t; u])
             when is elt Unif_rule.equiv -> (t, u) :: unpack w
        | _ -> assert false
      else if is elt Unif_rule.equiv then [(v, w)]
      else assert false
  | _ -> assert false

let pp_unif_rule : p_rule pp = fun ppf {elt=(l,r);_} ->
  let lhs =
    match Syntax.p_get_args l with
    | (_, [t; u]) -> (t, u)
    | _           -> assert false
  in
  out ppf "%a ↪ [%a]" equiv lhs (List.pp equiv "; ") (unpack r)
  *)
let pp_proof_end : p_proof_end pp = fun ppf pe ->
  out ppf (match pe.elt with
           | P_proof_end   -> "end"
           | P_proof_admitted -> "admitted"
           | P_proof_abort -> "abort")
(*
let ('term, 'binder) pp_rw_patt :
'term pp -> 'binder pp -> (('term, 'binder) rw_patt) pp
  = fun pp_t pp_b ppf rwp =
match rwp.elt with
  | Rw_Term ->
  | Rw_InTerm ->
  | Rw_InIdInTerm ->
  | Rw_IdInTerm ->
  | Rw_TermInIdInTerm ->
  | Rw_TermAsIdInTerm ->

let pp_p_rw_patt : p_rw_patt pp =
  (p_term, p_ident * p_term) pp_rw_patt pp_p_term pp_p_ident
  *)
let pp_rw_patt : p_rw_patt pp = fun ppf p ->
  match p.elt with
  | Rw_Term(t)               -> pp_p_term ppf t
  | Rw_InTerm(t)             -> out ppf "in %a" pp_p_term t
  | Rw_InIdInTerm(x,t)       -> out ppf "in %a in %a" pp_p_ident x pp_p_term t
  | Rw_IdInTerm(x,t)         -> out ppf "%a in %a" pp_p_ident x pp_p_term t
  | Rw_TermInIdInTerm(u,(x,t)) ->
      out ppf "%a in %a in %a" pp_p_term u pp_p_ident x pp_p_term t
  | Rw_TermAsIdInTerm(u,(x,t)) ->
      out ppf "%a as %a in %a" pp_p_term u pp_p_ident x pp_p_term t

let pp_p_assertion : p_assertion pp = fun ppf a ->
  match a with
  | P_assert_typing(t,a) -> out ppf "%a : %a" pp_p_term t pp_p_term a
  | P_assert_conv(t,u)   -> out ppf "%a ≡ %a" pp_p_term t pp_p_term u

let pp_query : p_query pp = fun ppf q ->
  match q.elt with
  | P_query_assert(true, a) -> out ppf "assertnot ⊢ %a" pp_p_assertion a
  | P_query_assert(false,a) -> out ppf "assert ⊢ %a" pp_p_assertion a
  | P_query_debug(true ,s) -> out ppf "set debug \"+%s\"" s
  | P_query_debug(false,s) -> out ppf "set debug \"-%s\"" s
  | P_query_flag(s, b) ->
      out ppf "set flag \"%s\" %s" s (if b then "on" else "off")
  | P_query_infer(t, _) -> out ppf "type %a" pp_p_term t
  | P_query_normalize(t, _) -> out ppf "compute %a" pp_p_term t
  | P_query_prover s -> out ppf "set prover \"%s\"" s
  | P_query_prover_timeout n -> out ppf "set prover_timeout %d" n
  | P_query_print None -> out ppf "print"
  | P_query_print(Some qid) -> out ppf "print %a" pp_p_qident qid
  | P_query_proofterm -> out ppf "proofterm"
  | P_query_verbose i -> out ppf "set verbose %i" i

let option_pp : 'a pp -> 'a option pp = fun pp_elt ppf o ->
  match o with None -> () | Some e -> pp_elt ppf e

let pp_tactic : p_tactic pp = fun ppf t ->
  begin match t.elt with
  | P_tac_admit -> out ppf "admit"
  | P_tac_apply t -> out ppf "apply %a" pp_p_term t
  | P_tac_assume ids ->
      let pp_p_param_id ppf x = out ppf " %a" pp_p_param_id x in
      out ppf "assume%a" (list_pp pp_p_param_id "") ids
  | P_tac_fail -> out ppf "fail"
  | P_tac_focus i -> out ppf "focus %i" i
  | P_tac_generalize id -> out ppf "generalize %a" pp_p_ident id
  | P_tac_have (id,t) -> out ppf "have %a: %a" pp_p_ident id pp_p_term t
  | P_tac_induction -> out ppf "induction"
  | P_tac_query q -> pp_query ppf q
  | P_tac_refine t -> out ppf "refine %a" pp_p_term t
  | P_tac_refl -> out ppf "reflexivity"
  | P_tac_rewrite(b,p,t)     ->
      let dir ppf b = if not b then out ppf " left" in
      let pat ppf p = out ppf " [%a]" pp_rw_patt p in
      out ppf "rewrite%a%a %a" dir b (option_pp pat) p pp_p_term t
  | P_tac_simpl None -> out ppf "simpl"
  | P_tac_simpl (Some qid) -> out ppf "simpl %a" pp_p_qident qid
  | P_tac_solve -> out ppf "solve"
  | P_tac_sym -> out ppf "symmetry"
  | P_tac_why3 p ->
      let prover ppf s = out ppf " \"%s\"" s in
      out ppf "why3%a" (option_pp prover) p
  end;
  out ppf ";"

(* Il n'y a pas de pp_p_symbol : il est codé dedans : *)

let pp_command : p_command pp = fun ppf {elt;_} ->
  begin match elt with
  | P_builtin(s,qid) -> out ppf "builtin \"%s\" ≔ %a" s pp_p_qident qid
  | P_inductive(_, _, []) -> assert false (* not possible *)
  | P_inductive(ms, xs, i::il) ->
      out ppf "%a%a%a%a"
        modifiers ms
        (list_pp pp_p_params " ") xs
        (pp_p_inductive "inductive") i
        (list_pp (pp_p_inductive "\nwith") "") il
  | P_notation(qid,n) -> out ppf "notation %a %a" pp_p_qident qid pp_notation n
  | P_open ps -> out ppf "open %a" (list_pp pp_p_path " ") ps
  | P_query q -> pp_query ppf q
  | P_require(b,ps) ->
      let op = if b then " open" else "" in
      out ppf "require%s %a" op (list_pp pp_p_path " ") ps
  | P_require_as(p,i) -> out ppf "require %a as %a" pp_p_path p pp_p_ident i
  | P_rules [] -> assert false (* not possible *)
  | P_rules (r::rs) ->
      out ppf "%a" (pp_p_rule "rule") r;
      List.iter (out ppf "%a" (pp_p_rule "\nwith")) rs
  | P_symbol
    {p_sym_mod;p_sym_nam;p_sym_arg;p_sym_typ;p_sym_trm;p_sym_prf;p_sym_def} ->
    begin
      out ppf "%asymbol %a%a%a" modifiers p_sym_mod pp_p_ident p_sym_nam
        pp_p_params_list p_sym_arg pp_p_typ p_sym_typ;
      if p_sym_def then out ppf " ≔";
      Option.iter (out ppf " %a" pp_p_term) p_sym_trm;
      match p_sym_prf with
      | None -> ()
      | Some(ts,pe) ->
          let pp_tactic ppf = out ppf "\n  %a" pp_tactic in
          out ppf "\nbegin%a\n%a" (list_pp pp_tactic "") ts pp_proof_end pe
    end
      (*| P_unif_rule(ur) -> out ppf "unif_rule %a" unif_rule ur *)
  | _ -> out ppf "UNIF_RULE not yet implemented"
  end;
  out ppf ";\n"

let pp_ast : ast pp = fun ppf ->
  Stream.iter (fun c -> pp_command ppf c; pp_print_newline ppf ())
