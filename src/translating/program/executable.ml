
open LP.Syntax

open Common.Type
open Common.Error
open Common.Xlib_OCaml

open Interface.LP_p_term
open Interface.K_prelude
open Interface.Signature

open Mecanism.Axiom_iterator

open TransSem.Axiom

let iter_exec : (string -> p_term) -> axiom -> signature -> p_term * (string list) StrMap.t =
  fun f_var ax sign_init ->
  let f_predicate_sym = sym_case in
  let f_predicate_var (n, p) s d = f_var n, s, d in
  let f_dom_val (sort, name) s d =
    create_ident name, s, (if sort = _SORT_ID then add_update_without_dup _SORT_ID name d else d) in
  let f_not _ _ _ =
    raise (KaMeLoError (NotYetImplemented, "Executable", "iter_exec", "Case Not"))            in
  let f_not_in _ _ _ =
    raise (KaMeLoError (NotYetImplemented, "Executable", "iter_exec", "Case Not-in"))         in
  let f_equals _ _ _ =
    raise (KaMeLoError (NotYetImplemented, "Executable", "iter_exec", "Case Equals"))         in
  let f_equals_dom _ _ _ =
    raise (KaMeLoError (NotYetImplemented, "Executable", "iter_exec", "Case Equals-dom_val")) in
  let f_and _ _ _ =
    raise (KaMeLoError (NotYetImplemented, "Executable", "iter_exec", "Case And"))            in
  let f_and_var _ _ _ =
    raise (KaMeLoError (NotYetImplemented, "Executable", "iter_exec", "Case And-var"))        in
  let res, _, free_var_data =
    axiom_iter_default_error [] ax f_var sign_init StrMap.empty
      f_predicate_sym f_predicate_var f_dom_val
      f_not f_not_in f_equals f_equals_dom f_and f_and_var
  in res, free_var_data

let iter_exec = iter_exec create_ident
