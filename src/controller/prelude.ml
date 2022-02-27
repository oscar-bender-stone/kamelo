open LP.Syntax

open Interface.LP_p_term
open Interface.K_prelude
open Interface.Getter_term

open Common.Error
open Mecanism.Count_data
open Translating.Prelude_data

(* let print_comment : output -> string -> unit = fun ppf message -> *)
let print_comment ppc message =
  print ppc "\n// " ; print ppc message ; print ppc "\n"

let pp_symbol_prelude ppc cd prt : p_symbol -> unit = fun sym ->
  incr_real_symbol cd ;
  (match sym.p_sym_typ with
   | Some v ->
      Translating.Viry.symb_signature :=
        Translating.Axiom.StrMap.add sym.p_sym_nam.elt v !Translating.Viry.symb_signature
   | None -> ()) ; prt ppc (no_pos (P_symbol sym))

let pp_sort_prelude ppc cd prt : p_symbol -> unit = fun sym ->
  incr_real_symbol cd ;
  (* (match sym.p_sym_typ with
   | Some v ->
      Translating.Axiom.sort_signature :=
        Translating.Axiom.StrMap.add sym.p_sym_nam.elt v !Translating.Axiom.sort_signature
   | None -> ()) ; *) prt ppc (no_pos (P_symbol sym))


let pp_builtin_prelude ppc _ prt : p_command -> unit = fun b -> prt ppc b

let pp_rule_prelude ppc _ prt : p_rule -> unit = fun r ->
  prt ppc (no_pos (P_rules [r]))

let create_prelude ppc prt : string -> unit = fun _ ->
  let cd = Mecanism.Count_data.reset_count_data 0 in
  let pp_sort = pp_sort_prelude ppc cd prt in
  let pp_symb = pp_symbol_prelude ppc cd prt in
  let pp_b = pp_builtin_prelude ppc cd prt in
  let pp_r = pp_rule_prelude ppc cd prt in
  (* STEP 1: The injection _INJD: injective symbol δ : SortK → TYPE; *)
  print_comment ppc "Our injection between K and Dedukti";
  pp_symb (create_p_symbol [no_pos (P_prop Injec)] "δ" []
             (Some (create_arrow p_SORTK p_TYPE)) None) ;
  (* Hooked-sort *)
  print_comment ppc "Translation of hooked sorts";
  List.iter (fun n -> pp_sort (create_symbol n p_SORTK)) hooked_sort ;
  (* STEP 2: Some constructors and builtin *)
     print_comment ppc "Some builtins for Lambdapi and constructors";
     (* For inductive type *)
     (* symbol Prop : TYPE; *)
     pp_symb (create_symbol "Prop" p_TYPE) ;
     (* symbol P : Prop → TYPE; *)
     pp_symb (create_symbol "P" (create_arrow (create_ident "Prop") p_TYPE)) ;
     (* builtin "Prop" ≔ Prop; *)
     pp_b (create_builtin_command "Prop" ([], "Prop")) ;
     (* builtin "P" ≔ P; *)
     pp_b (create_builtin_command "P" ([], "P")) ;
     print ppc "\n";
     (* symbol true : injK SortBool; *)
     pp_symb (create_symbol "true" (wrap "SortBool")) ;
     (* symbol false : injK SortBool; *)
     pp_symb (create_symbol "false" (wrap "SortBool")) ;
     (* constant symbol zero : injK SortInt; *)
     pp_symb (create_symbol "zero" (wrap "SortInt")) ;
     (* constant symbol succ : injK SortInt → injK SortInt; *)
     pp_symb (create_symbol "succ" (create_arrow (wrap "SortInt") (wrap "SortInt"))) ;
     print ppc "\n";
     (* builtin "0"  ≔ zero; *)
     pp_b (create_builtin_command "0" ([], "zero")) ;
     (* builtin "+1" ≔ succ; *)
     pp_b (create_builtin_command "+1" ([], "succ")) ;
     (* STEP 3: Hooked-symbol *)
     print_comment ppc "Translation of hooked symbols";
     let f (n,l) = pp_symb (create_symbol n (create_type_arrow (n,l))) in
     List.iter f hooked_symbol ;
     (* STEP 4: Add semantic rules *)
     print_comment ppc "Translation of semantic rules";
     let g ((hl, bl), (hr, br)) =
       pp_r (no_pos (List.fold_left create_appl hl bl,
                     List.fold_left create_appl hr br))
     in
     List.iter g (semantic_rule())
