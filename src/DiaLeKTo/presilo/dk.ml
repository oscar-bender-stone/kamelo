(** The printer for Dedukti. *)

open Common_presilo

open Common.Error
open Type.Command
open Type.Term

open Constructor.Command

(* TODO *)
type name = string
let pp_dk_name f ppc : name -> unit = fun n -> print ppc "%s" (f n)

let pp_ident f ppc : name -> unit = fun n -> print ppc "%s" (f n)

(* let pp_term f ppc : term -> unit = fun t -> print ppc "%s" (f t) *)

let pp_dk_sep ppc = print ppc "%s" ".\n"


let rec pp_term f ppc : term -> unit = fun te ->
  match te with
  | Type -> print ppc "Type"
  (*  | DB (_, x, n) -> print ppc "%a[%i]" pp_ident x n *)
  | Sym n -> print ppc "%a" (pp_dk_name f) n
  | Var n -> print ppc "%a" (pp_dk_name f) n
  | Appl (t, t_l) -> pp_list " " (pp_term_wp f) ppc (t :: t_l)

  | Lambda ((x, None), t) ->
     (* print ppc "%a => %a" (pp_ident f) x pp_term t *)
     pp_list " => " (pp_term_wp f) ppc (t :: (List.map (fun x -> Sym x) x))
(*  | Lambda ((x, Some a), t) ->
     print ppc "%a:%a => %a" (pp_ident f) x (pp_term_wp f) a pp_term t

(*  | Pi ((x, a), b) when ident_eq x dmark ->
      print ppc "%a -> %a" (pp_term_wp f) a pp_term b *)
  | Pi ((x, a), b) ->
      print ppc "%a:%a -> %a" (pp_ident f) x (pp_term_wp f) a pp_term b

  | Arrow (t1, t2) ->
     pp_term ppc (to_full_pi te)
     *)
  | _ -> failwith "TODO"

and pp_term_wp f ppc : term -> unit = fun te ->
  match te with
  | (Type | Sym _ | Var _) as t -> pp_term f ppc t
  | t -> print ppc "(%a)" (pp_term f) t


let  pp_pattern _ _ : pattern -> unit =
  failwith "TODO"

(*
let rec pp_pattern out pattern =
  match pattern with
  | Var (_, x, n, [])    -> fprintf out "%a[%i]" pp_ident x n
  | Var (_, x, n, lst)   ->
      fprintf out "%a[%i] %a" pp_ident x n (pp_list " " pp_pattern_wp) lst
  | Pattern (_, n, [])   -> fprintf out "%a" pp_name n
  | Pattern (_, n, pats) ->
      fprintf out "%a %a" pp_name n (pp_list " " pp_pattern_wp) pats
  | Lambda (_, x, p)     -> fprintf out "%a => %a" pp_ident x pp_pattern p
  | Brackets t           -> fprintf out "{ %a }" pp_term t

and pp_pattern_wp out pattern =
  match pattern with
  | (Var (_, _, _, _ :: _) | Pattern _ | Lambda _) as p ->
      fprintf out "(%a)" pp_pattern p
  | p -> pp_pattern out p
 *)

(** ********************* *)
(**  Logic statement      *)
(** ********************* *)

(** ***************** *)
(** A. Symbol         *)
(** ***************** *)

(** Parsing *)

(** [pp_dk_mixfix ppc m] prints a mixfix [m]. *)
let pp_dk_mixfix ppc : mixfix -> unit = function
  | Infix _ -> wrn_dk ppc "infix symbol"   "a prefix symbol" ; print ppc ""
  | Prefix  -> print ppc ""
  | Postfix -> wrn_dk ppc "postfix symbol" "a prefix symbol" ; print ppc ""
  | Closed  -> wrn_dk ppc "closed symbol"  "a prefix symbol" ; print ppc ""

(** [pp_dk_parsing_rule ppc pr] prints a parsing rule [pr],
     i.e. warning(s) or nothing *)
let pp_dk_parsing_rule ppc : parsing_rule -> unit = fun pr ->
  pp_dk_mixfix ppc pr.mixfix ;
  if pr.prec <> v_default_prec then
    wrn_dk ppc "precedence feature" "the default precedence (the only one)"

(** Visibility *)

(** [pp_dk_visibility ppc v] prints a visibility [v]. *)
let pp_dk_visibility ppc : visibility -> unit = function
  | Private   -> print ppc "private "
  | Public    -> print ppc ""
  | Protected -> wrn_dk ppc "visibility Protected" "the visibility Public" ;
                 print ppc ""

(** Property *)

(** [pp_dk_property ppc p] prints a property [p]. *)
let pp_dk_property ppc : bool -> property -> unit = fun opaq p -> match p with
  | Static            -> print ppc ""
  | Definable Free    -> print ppc (if opaq then "thm " else "def ")
  | Definable C       ->
     wrn_dk ppc "property commutative" "a definable symbol" ; print ppc "def "
  | Definable AC      -> print ppc "defac "
  | Definable (ACU _) -> print ppc "defacu " (* TODO element neutre *)
  (* fprintf fmt "@[<2>%sdefacu %a [@ %a, %a].@]@.@."(scope_to_string scope)
     print_ident id print_term ty print_term neu *)
  | Injective         -> print ppc "inj "

(** Main type *)

let pp_dk_type_decl f ppc : type_decl -> unit = fun t -> (* TODO fix *)
  print ppc "%a : %a" (pp_dk_name f) "" (pp_term f) (snd t)

let pp_wrap_dk_sym_identity f ppc : bool -> sym_identity -> unit = fun opaq sym_id ->
  pp_dk_parsing_rule ppc sym_id.parsing    ;
  pp_dk_visibility   ppc sym_id.visibility ;
  pp_dk_property     ppc opaq sym_id.prop  ;
  pp_dk_name       f ppc sym_id.name

let pp_dk_sym_decl f ppc : sym_decl -> unit = fun d ->
  pp_wrap_dk_sym_identity f ppc false d.sym ;
  pp_dk_type_decl         f ppc d.typ

(** ***************** *)
(** B. Definition     *)
(** ***************** *)

let pp_dk_def_body f ppc : def_body -> unit = function
  | LambdaTerm t -> pp_term f ppc t
  | Script _     -> wrn_dk ppc "script of tactics" "nothing.."

let pp_dk_def_decl f ppc : def_decl -> unit = fun d ->
  pp_wrap_dk_sym_identity f ppc d.opacity d.sym ;
  (match d.typ with
   | None -> ()
   | Some t -> pp_dk_type_decl f ppc t) ;
  print ppc " := " ; pp_dk_def_body f ppc d.def

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

let pp_dk_rule f ppc : rule -> unit = fun (l,r) ->
  print ppc "[] %a --> %a." (* TODO ajout arg entre [%a] *) (pp_pattern f) l (pp_term f) r ; pp_dk_sep ppc

(* TODO Pattern ? Bracket ? *)

let pp_dk_logic_stmt f ppc : logic_statement -> unit = function
  | Symbol s     -> pp_dk_sym_decl f ppc s ; pp_dk_sep ppc
  | Definition d -> pp_dk_def_decl f ppc d ; pp_dk_sep ppc
  | Rule r_l     -> List.iter (pp_dk_rule f ppc) r_l

(* Mieux avec @[ ?

  | Symbol (_, id, scope, Static, ty) ->
     print ppc "@[<2>%s%a :@ %a.@]@.@." (scope_to_string scope) pp_ident id
        pp_term ty
  | Symbol (_, id, scope, Definable Free, ty) ->
      print ppc "@[<2>%sdef %a :@ %a.@]@.@." (scope_to_string scope) pp_ident
        id pp_term ty
  | Symbol (_, id, scope, Injective, ty) ->
      print ppc "@[<2>%sinjective %a :@ %a.@]@.@." (scope_to_string scope)
      pp_ident id pp_term ty
  | Symbol (_, id, scope, Definable AC, ty) ->
      print ppc "@[<2>%sdefac %a [@ %a].@]@.@." (scope_to_string scope)
      pp_ident id pp_term ty
  | Symbol (_, id, scope, Definable (ACU neu), ty) ->
      printf ppc "@[<2>%sdefacu %a [@ %a, %a].@]@.@." (scope_to_string scope)
        pp_ident id pp_term ty pp_term neu
  | Definition (_, id, scope, opaque, ty, te) -> (
      let key = if opaque then "thm" else "def" in
      match ty with
      | None    ->
          print ppc "@[<hv2>%s%s %a@ :=@ %a.@]@.@." (scope_to_string scope)
            key pp_ident id pp_term te
      | Some ty ->
          print ppc "@[<hv2>%s%s %a :@ %a@ :=@ %a.@]@.@."
            (scope_to_string scope) key pp_ident id pp_term ty pp_term te)

  | Rule [] -> assert false (* not possible *)
  | Rule (r :: rs) ->
     pp_lp_rule f ppc "rule" r ;
     List.iter (pp_lp_rule f ppc "with") rs
 *)

(** ********************* *)
(**  Set option           *)
(** ********************* *)

let pp_dk_set_option ppc : set_option -> unit = fun _ ->
  wrn_dk ppc "set options" "nothing.." (* TODO #DEBUG ? *)

(** ********************* *)
(**  Query                *)
(** ********************* *)

let pp_dk_strategy ppc : strat -> unit = function
  | SNF  -> print ppc "SNF"
  | WHNF -> print ppc "WHNF"

let pp_dk_config ppc : config -> unit = function
  | None, None     -> print ppc ""
  | Some n, None   -> print ppc "[%i]" n
  | None, Some s   -> print ppc "[%a]" pp_dk_strategy s
  | Some n, Some s -> print ppc "[%i, %a]" n pp_dk_strategy s

let pp_dk_operation f t1 t2 ppc : op -> unit = fun o ->
  let f_print b s =
    print ppc s (if b then "" else "NOT") (pp_term f) t1 (pp_term f) t2
  in
  match o with
  | Conv b    -> f_print b "%s %a == %a"
  | HasType b -> f_print b "%s %a :: %a"

let pp_dk_query f ppc : query -> unit = fun q -> match q with
   | Eval (cfg, t) ->
      print ppc "#EVAL%a %a"  pp_dk_config cfg (pp_term f) t ; pp_dk_sep ppc
   | Infer(cfg, t) ->
      print ppc "#INFER%a %a" pp_dk_config cfg (pp_term f) t ; pp_dk_sep ppc
   | Check (op, t1, t2) ->
      print ppc "#CHECK%a"  (pp_dk_operation f t1 t2) op ; pp_dk_sep ppc
   | Assert(op, t1, t2) ->
      print ppc "#ASSERT%a" (pp_dk_operation f t1 t2) op ; pp_dk_sep ppc

   | DTree -> () (* (_, m, v) -> TODO
     (match m with
      | None   -> print ppc "#GDT %a.@." pp_ident v
      | Some m -> print ppc "#GDT %a.%a.@." pp_mident m pp_ident v) *)

   | Print(opt, t) ->
      (match opt, t with
       | None, None | Some ProofTerm, None ->
          wrn_dk ppc "proof development" "nothing.."
       | Some Goal, None ->
          wrn_dk ppc "proof development" "nothing.."
       | None, Some t ->
          print ppc "#EVAL[0] %a" (pp_term f) t ; pp_dk_sep ppc
       | _,_ -> wrn_msg ppc "Bad command: do nothing")
   | SPrint(s) -> print ppc "#PRINT %S" s ; pp_dk_sep ppc

(** ********************* *)
(**  (Un)safe command     *)
(** ********************* *)
(*
let rec pp_dk_path ppc : path list -> unit = function (* TODO *)
  | []   -> ""
  | [t]  -> t
  | t::q -> wrn_dk ; pp_dk_path ppc q

let pp_dk_import ppc : import_decl -> unit = function
  | Require of bool * p_path list (* "require open" if the boolean is true *)
  | Require_as of p_path * p_ident
  | Open    of p_path list
 *)

let pp_dk_command f ppc : command -> unit = function
  | Import  _ -> ()
  (* TODO  | Require (_, md) -> print ppc "#REQUIRE %a.@." pp_mident md *)
  | Comment s -> print ppc "(; %s ;)\n" s
  | Builtin _ -> wrn_dk ppc "builtin" "nothing.."
  | Inductive  _ -> () (* TODO *)
  | Logic_stmt s -> pp_dk_logic_stmt f ppc s
  | Set_option o -> pp_dk_set_option ppc o
  | Query q -> pp_dk_query f ppc q


let pp_dk_unsafe f ppc : unsafe_command -> unit = fun unsafe_c ->
  wrn_msg ppc ("No unsafe command in Dedukti") ; match unsafe_c with
  | Unif_rule    _ -> wrn_dk ppc "unification rule" "nothing.."
  | Sequential   s ->
     wrn_dk ppc "sequential symbol" "a symbol, no sequential one" ;
     pp_dk_sym_decl f ppc s
  | Safe_command c -> pp_dk_command f ppc c
