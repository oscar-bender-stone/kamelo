(** The printer for Lambdapi. *)

open Common_presilo

open Common.Error
open Type.Command
open Type.Term

open Constructor.Command

(* TODO *)
type name = string
let pp_name f ppc : name -> unit = fun n -> print ppc "%s" (f n)


(* let pp_term f ppc : term -> unit = fun t -> print ppc "%s" (f t) *)


(*
let list_pp : 'a pp -> string -> 'a list pp = fun pp_elt sep ->
  Format.pp_print_list ~pp_sep:(pp_sep sep) pp_elt

let pp_path : path pp = list_pp pp_uid "."

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

 *)
let rec pp_term f ppc : term -> unit = fun te ->
  match te with
  | Type -> print ppc "TYPE"
  (*  | DB (_, x, n) -> print ppc "%a[%i]" pp_ident x n *)
  | Sym n -> print ppc "%a" (pp_name f) n
  | Var n -> print ppc "%a" (pp_name f) n

  | Appl (t, t_l) -> pp_list " " (pp_term_wp f) ppc (t :: t_l)

  | Lambda ((x, None), t) ->
     print ppc "λ" ;
     pp_list " " (pp_name f) ppc x ;
     print ppc ", %a" (pp_term f) t
  | Lambda ((x, Some a), t) ->
     print ppc "λ" ;
     pp_list " " (pp_name f) ppc x ;
     print ppc ":%a, %a" (pp_term_wp f) a (pp_term f) t

  | Pi ((x, None), b) ->
     print ppc "Π" ;
     pp_list " " (pp_name f) ppc x ;
     print ppc ", %a" (pp_term f) b
  | Pi ((x, Some a), b) ->
     print ppc "Π" ;
     pp_list " " (pp_name f) ppc x ;
     print ppc ":%a, %a" (pp_term_wp f) a (pp_term f) b

  | Arrow (t1, t2) ->
     print ppc "%a → %a" (pp_term f) t1 (pp_term f) t2

and pp_term_wp f ppc : term -> unit = fun te ->
  match te with
  | (Type | Sym _ | Var _) as t -> pp_term f ppc t
  | t -> print ppc "(%a)" (pp_term f) t

(*

let rec pp_p_term : p_term pp = fun ppf t ->


    | (P_Iden(qid,false)   , _    ) -> out ppf "%a" pp_p_qident qid

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

    | (_                   , _    ) -> out ppf "(%a)" func t
  in
  let rec toplevel ppf t =
    match t.elt with
    | P_Abst(xs,t) -> out ppf "λ%a, %a" pp_p_params_list xs toplevel t
    | P_Prod(xs,b) -> out ppf "Π%a, %a" pp_p_params_list xs toplevel b
    | P_Arro(a,b) -> out ppf "%a → %a" appl a toplevel b
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

  *)


let rec pp_pattern f ppc : pattern -> unit = function
  | Wildcard -> print ppc "_"
  | Var v    -> print ppc "$%a" (pp_name f) v
  | Pattern(i, patt_l) ->
     print ppc "%a " (pp_name f) i ;
     pp_list " " (pp_pattern f) ppc patt_l

let pp_lp_sep ppc = print ppc "%s" ";\n"

(** ********************* *)
(**  Logic statement      *)
(** ********************* *)

(** ***************** *)
(** A. Symbol         *)
(** ***************** *)

(** Parsing *)

(** [pp_lp_associativity ppc a] prints an associativity [a]. *)
let pp_lp_associativity ppc : associativity -> unit = function
  | Left      -> print ppc "left "
  | Right     -> print ppc "right "
  | Non_assoc -> print ppc ""

(** [pp_lp_mixfix ppc m] prints a mixfix [m]. *)
let pp_lp_mixfix ppc : mixfix -> unit = function
  | Infix a -> print ppc "infix %a" pp_lp_associativity a
  | Prefix  -> print ppc "prefix "
  | Postfix -> wrn_lp ppc "postfix symbol" "a prefix symbol" ;
               print ppc "prefix "
  | Closed  -> wrn_lp ppc "closed symbol"  "a prefix symbol" ;
               print ppc "prefix "
  (* TODO | Quant        -> print ppc "quantifier"
  | _ -> () *)

(** [pp_lp_parsing_rule ppc pr] prints a parsing rule [pr],
     i.e. the mixifx and the precedence,
     i.e. "infix left 6" in "notation + infix left 6;" *)
let pp_lp_parsing_rule ppc : parsing_rule -> unit = fun pr ->
  pp_lp_mixfix ppc pr.mixfix ; print ppc "%i" pr.prec

(** Visibility *)

(** [pp_lp_visibility ppc v] prints a visibility [v]. *)
let pp_lp_visibility ppc : visibility -> unit = function
  | Private   -> print ppc "private "
  | Public    -> print ppc ""
  | Protected -> print ppc "protected "

(** Property *)

(** [pp_lp_property ppc p] prints a property [p]. *)
let pp_lp_property ppc : property -> unit = function
  | Static            -> print ppc "constant "
  | Definable Free    -> print ppc ""
  | Definable C       -> print ppc "commutative "
  | Definable AC      -> print ppc "left associative commutative "
  (* TODO | AC false -> print ppc "associative commutative" *)
  | Definable (ACU _) -> wrn_lp ppc "property ACU" "the property AC only" ;
                         print ppc "left associative commutative "
  | Injective         -> print ppc "injective "

(** Main type *)

let pp_wrap_sym_identity f ppc :
      (unit -> unit) -> (unit -> unit) -> sym_identity -> unit =
  fun pp_wrap_begin pp_wrap sym_id ->
  pp_wrap_begin () ;
  pp_lp_visibility ppc sym_id.visibility ;
  pp_lp_property   ppc sym_id.prop       ;
  print ppc "symbol " ; pp_name f ppc sym_id.name ; pp_wrap() ; pp_lp_sep ppc ;
  if not(sym_id.parsing.mixfix = v_default_mixfix && sym_id.parsing.prec = v_default_prec) then
    (print ppc "notation %a %a" (pp_name f) sym_id.name pp_lp_parsing_rule sym_id.parsing ;
     pp_lp_sep ppc)
(* | P_notation (qid, n) -> print ppc "notation %a %a" pp_qident qid pp_notation n *) (* TODO possibilité d'enlever "notation" *)

let pp_lp_type_decl f ppc : type_decl -> unit = fun t ->
  (match fst t with
   | [] -> ()
   | _::_ -> () (* TODO *) ) ;
  print ppc " : %a" (pp_term f) (snd t)

let pp_lp_sym_decl f ppc : sym_decl -> unit = fun d ->
  pp_wrap_sym_identity f ppc (fun () -> ()) (fun () -> pp_lp_type_decl f ppc d.typ) d.sym

(** ***************** *)
(** B. Definition     *)
(** ***************** *)

(* TODO *)
let pp_iden f ppc : iden -> unit = fun s -> print ppc "%s" (f s)
let rec pp_list f ppc : iden option list -> unit = function
  | [] -> ()
  | None::q -> pp_list f ppc q
  | (Some t)::q -> print ppc "%a" (pp_iden f) t ; pp_list f ppc q

let pp_lp_tactic f ppc : tactic -> unit = function (* TODO *)
  | Admit    -> print ppc "admit"
  | Refine t -> print ppc "refine %a" (pp_term f) t
  | Assume ids ->
  (* print ppc "assume%a" (List.pp (pp_unit " " |+ param_id) "") ids *)
     print ppc "assume%a" (pp_list f) ids
  | Apply t  -> print ppc "apply %a" (pp_term f) t
  | Generalize id -> print ppc "generalize %a" (pp_iden f) id
  | Have (id, t) -> print ppc "have %a: %a" (pp_iden f) id (pp_term f) t
  | Simpl None -> print ppc "simpl"
  | Simpl (Some id) -> print ppc "simpl %a" (pp_iden f) id
  | Rewrite(b, (*p, *) t)     ->
      let dir ppc b = if not b then print ppc " left" in
      (* let pat ppf p = print ppc " [%a]" pp_rw_patt p in
      print ppc "rewrite%a%a %a" dir b (Option.pp pat) p (pp_term f) t *)
      print ppc "rewrite%a %a" dir b (pp_term f) t
  | Reflexivity -> print ppc "reflexivity"
  | Symmetry -> print ppc "symmetry"
  | Induction -> print ppc "induction"
  | Solve -> print ppc "solve"
  | Why3 None     -> print ppc "why3 \"Alt-Ergo\"" (* TODO *)
  | Why3 (Some p) -> print ppc "why3 \"%s\"" p (* TODO *)
  | Focus i -> print ppc "focus %i" i
  | Fail -> print ppc "fail"
 (* | Query q -> pp_query f ppc q *)


let pp_lp_proof_ending ppc : proof_ending -> unit = function
  | End      -> print ppc "end"
  | Admitted -> print ppc "admitted"
  | Abort    -> print ppc "abort"

let pp_lp_def_body f ppc : def_body -> unit = function
  | LambdaTerm t           -> pp_term f ppc t
  | Script None            -> print ppc ""
  | Script (Some (t_l, e)) -> (* TODO fix *)
     List.iter (pp_lp_tactic f ppc) t_l ; pp_lp_proof_ending ppc e

let pp_lp_def_decl f ppc : def_decl -> unit = fun d ->
  let pp_wrap_begin () = print ppc (if d.opacity then "opaque " else "") in
  let pp_wrap () =
    match d.typ with
    | None -> ()
    | Some t -> pp_lp_type_decl f ppc t
  in
  pp_wrap_sym_identity f ppc pp_wrap_begin pp_wrap d.sym ;
  print ppc " := " ; pp_lp_def_body f ppc d.def

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

let pp_lp_rule f ppc : string -> rule -> unit = fun kw (l,r) ->
  print ppc "%s %a ↪ %a\n" kw (pp_pattern f) l (pp_term f) r

let pp_lp_logic_stmt f ppc : logic_statement -> unit = function
  | Symbol s     -> pp_lp_sym_decl f ppc s
  | Definition d -> pp_lp_def_decl f ppc d
  | Rule []      -> ()
  | Rule [t]     -> pp_lp_rule f ppc "rule" t ; pp_lp_sep ppc
  | Rule (t::q)  ->
     pp_lp_rule f ppc "rule" t ;
     List.iter (pp_lp_rule f ppc "with") q ; pp_lp_sep ppc

(** ********************* *)
(**  Set option           *)
(** ********************* *)

let pp_lp_flag_opt ppc : flag_opt -> unit = function
  | Print_opt Implicit  -> print ppc "print_implicits "
  | Print_opt Context   -> print ppc "print_contexts "
  | Print_opt Domain    -> print ppc "print_domains "
  | Print_opt Meta_type -> print ppc "print_meta_types "
  | Print_opt Meta_arg  -> print ppc "print_meta_args "
  | Rewrite_opt Eta_equalify -> print ppc "eta_equality "

let pp_lp_prover ppc : prover -> unit = function
  | EProver  -> print ppc "EProver"
  | Alt_Ergo -> print ppc "Alt-Ergo"

let pp_lp_set_option ppc : set_option -> unit = function
  | Debug(true ,s) ->
     print ppc "set debug \"+%s\"" s ; pp_lp_sep ppc
  | Debug(false,s) ->
     print ppc "set debug \"-%s\"" s ; pp_lp_sep ppc
  | Verbosity i    ->
     print ppc "set verbose %i" i ; pp_lp_sep ppc
  | Flag(o, b) ->
     print ppc "set flag \"%a\" %s" pp_lp_flag_opt o
       (if b then "on" else "off") ; pp_lp_sep ppc
  | Prover(None, None) -> ()
  | Prover(Some p, Some i) ->
     print ppc "set prover \"%a\"" pp_lp_prover p ;
     print ppc "set prover_timeout %d" i ; pp_lp_sep ppc
  | Prover(Some p, None) ->
     print ppc "set prover \"%a\"" pp_lp_prover p ; pp_lp_sep ppc
  | Prover(None, Some i) ->
     print ppc "set prover_timeout %d" i ; pp_lp_sep ppc

(** ********************* *)
(**  Query                *)
(** ********************* *)

let pp_operation f t1 t2 ppc : op -> unit = fun o ->
  let f_print b s =
    print ppc s (if b then "" else "not") (pp_term f) t1 (pp_term f) t2
  in
  match o with
  | Conv b    -> f_print b "%s ⊢ %a : %a"
  | HasType b -> f_print b "%s ⊢ %a ≡ %a"

let pp_lp_query f ppc : query -> unit = fun q ->
  match q with
  | Eval ((None, None), t)     ->
     print ppc "compute %a" (pp_term f) t ; pp_lp_sep ppc
  | Eval ((Some 0, None), t)   ->
     print ppc "print %a" (pp_term f) t ; pp_lp_sep ppc
  | Eval ((Some 0, Some _), t) ->
     wrn_lp ppc "strategies of compute" "a printing command (because of step = 0)" ;
     print ppc "print %a" (pp_term f) t ; pp_lp_sep ppc
  | Eval((_, _), t) ->
     wrn_lp ppc "options of compute" "a compute command without option" ;
     print ppc "compute %a" (pp_term f) t ; pp_lp_sep ppc
  | Infer((None, None), t) ->
     print ppc "type %a" (pp_term f) t ; pp_lp_sep ppc
  | Infer((_, _), t) ->
     wrn_lp ppc "options of type" "a type command without option" ;
     print ppc "type %a" (pp_term f) t ; pp_lp_sep ppc
  | Check (op, t1, t2) ->
     wrn_lp ppc "command check" "an assert command" ;
     print ppc "assert%a" (pp_operation f t1 t2) op ; pp_lp_sep ppc
  | Assert(op, t1, t2) ->
     print ppc "assert%a" (pp_operation f t1 t2) op ; pp_lp_sep ppc
  | DTree -> wrn_lp ppc "printing of decision tree" "nothing.." ; ()
  | Print(opt, t) ->
     (match opt, t with
      | None, None | Some ProofTerm, None ->
         print ppc "proofterm;" ; pp_lp_sep ppc
      | Some Goal, None ->
         print ppc "print" ; pp_lp_sep ppc
      | None, Some t ->
         print ppc "print %a" (pp_term f) t ; pp_lp_sep ppc
      | _,_ -> wrn_msg ppc "Bad command: do nothing")
  | SPrint _ -> wrn_lp ppc "printing of string" "nothing.."

(** ********************* *)
(**  (Un)safe command     *)
(** ********************* *)

(*
let pp_lp_path ppc : path list -> unit = (* TODO *)

let pp_lp_import ppc : import_decl -> unit = function
  | Require(b, path)
      print ppc "require%a %a"
        (pp_if b (pp_unit " open")) ()
        (List.pp pp_lp_path " ") pp_lp_path
  | P_require_as (p,i) ->
     print ppc "@[require %a@ as %a@]" pp_lp_path p ident i
  |P_open ps -> print ppc "open %a" (List.pp path " ") ps
 *)

let pp_lp_builtin_opt ppc : builtin_opt -> unit = function
  | Zero   -> print ppc "0"
  | Succ   -> print ppc "+1"
  | Set    -> print ppc "T"
  | Prop   -> print ppc "P"
  | Eq     -> print ppc "eq"
  | Refl   -> print ppc "refl"
  | Eq_ind -> print ppc "eq_ind"
  | Top    -> print ppc "top"
  | Bot    -> print ppc "bot"
  | Not    -> print ppc "not"
  | Or     -> print ppc "or"
  | And    -> print ppc "and"
  | Imp    -> print ppc "imp"

let pp_lp_builtin f ppc : builtin_opt * name -> unit = fun (b,n) ->
  (* TODO *)
  print ppc "@[builtin \"%a\"@ ≔ %a@]" pp_lp_builtin_opt b (pp_name f) n ;
  pp_lp_sep ppc
(*
let inductive : string -> p_inductive pp =
  let cons ppf (id,a) = out ppf "| %a : %a" ident id term a in
  fun kw ppf {elt=(id,a,cs);_} ->
  out ppf "@[<v>%s %a : %a ≔@,%a@]" kw ident id term a (List.pp cons "@,") cs
(** TODO *)

  | P_inductive (_, _, []) -> assert false (* not possible *)
  | P_inductive (ms, xs, i :: il) ->
      out ppf "@[<v>@[%a%a@]%a@,%a@,end@]"
        modifiers ms
        (List.pp params " ") xs
        (inductive "inductive") i
        (List.pp (inductive "with") "@,") il
      *)

let pp_lp_command f ppc : command -> unit = function
  | Import  _ -> ()
  | Comment s -> print ppc "// %s" s ; pp_lp_sep ppc
  | Builtin (b,n) -> pp_lp_builtin f ppc (b,n)
  | Inductive  _  -> ()
  | Logic_stmt s  -> pp_lp_logic_stmt f ppc s
  | Set_option o  -> pp_lp_set_option ppc o
  | Query q -> pp_lp_query f ppc q

let pp_lp_unsafe f ppc : unsafe_command -> unit = function
  | Unif_rule ur   -> (* TODO fix *)
     print ppc "unif_rule %a;" (fun ppc -> pp_lp_rule f ppc "rule") ur
  | Sequential s   -> print ppc "sequential " ;
                      pp_lp_sym_decl f ppc s ; pp_lp_sep ppc
  | Safe_command c -> pp_lp_command f ppc c
