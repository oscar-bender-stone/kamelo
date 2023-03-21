
open Common.Type
open Common.Error
open Common.Xlib_OCaml
open Mecanism.Kommand_iterator
open Interface.Output

type output = Format.formatter

let verbose = ref false

let fresh = ref 0
let fresh_name_ax ppc () = print ppc "symbol ax%i " !fresh ; incr fresh

let pp_endline ppc = print ppc "\n"
let pp_paren   ppc = print ppc ")"
let space ppc : unit = print ppc "  "
let rec alignment ppc : int -> unit = fun n ->
  if n <= 0 then () else (space ppc ; alignment ppc (n-1))

let pp_list ppc : string -> (output -> 'a -> unit) -> 'a list -> string -> string -> unit =
  fun first f l separator last ->
  let prints = print ppc "%s" in
  prints first ;
  let rec aux l = match l with
    | []  -> prints " "
    | [t] -> f ppc t
    | t1::t2::q -> f ppc t1 ; prints separator ; aux (t2::q)
  in
  aux l ; prints last

let pp_kore_param ppc : param -> unit = fun p -> match p with
  | S s  -> print ppc "%s" (pp s)
  | Q qv -> print ppc "%s" (pp qv)

let pp_kore_quant_var_list ppc : quant_var list -> unit = fun qv_l ->
  let f_qv ppc qv = print ppc "%s" (pp qv) in
  match qv_l with
  | [] -> ()
  | _  -> pp_list ppc "" f_qv qv_l " " " "

let pp_kore_param_list ppc : param list -> unit = fun p_l ->
  match p_l with
  | []  -> print ppc " : iK "
  | _   -> pp_list ppc " : iK " pp_kore_param p_l " → iK " " → iK "

let pp_kore_param_list_bis ppc : param list -> unit = fun p_l ->
  match p_l with
  | [] -> ()
  | _  -> pp_list ppc "" pp_kore_param p_l  " " " "

let pp_kore_attribute ppc : attribute -> unit = fun attr ->
  let print = print ppc "%s" in
  match attr with
  | Assoc       _ -> print "ASSOC"
  | Comm        _ -> print "COMM"
  | Idem        _ -> print "IDEM"
  | Unit        _ -> print "UNIT"

  | Strict      _ -> print "STRICT"
  | Seqstrict   _ -> print "SEQSTRICT"

  | Cool        _ -> print "COOL"
  | CoolLike    _ -> print "COOL-LIKE"
  | Heat        _ -> print "HEAT"
  | Structural  _ -> print "STRUCTURAL"

  | Simpl       _ -> print "SIMPLIFICATION"

  | Left        _ -> print "LEFT"
  | Right       _ -> print "RIGHT"
  | Priority    _ -> print "PRIORITY"
  | Priorities  _ -> print "PRIORITIES"

  | Constructor _ -> print "CONSTRUCTOR"
  | Injective   _ -> print "INJECTIVE"
  | Predicate   _ -> print "PREDICATE"

  | Total       _ -> print "TOTAL"
  | Function    _ -> print "FUNCTION"

  | Anywhere    _ -> print "ANYWHERE"
  | Owise       _ -> print "OWISE"

  | Subsort     _ -> print "SUBSORT"
  | Projection  _ -> print "PROJECTION"
  | Initializer _ -> print "INITIALIZER"

  | Other(s, _)   -> print s

  | Unique      _ -> print "UNIQUE ID"
  | Location    _ -> print "LOCATION"
  | Source      _ -> print "SOURCE"

let pp_kore_attribute_list ppc : attribute list -> unit = fun attr_l ->
  pp_list ppc "//" pp_kore_attribute attr_l  " " ""

let pp_kore_sort ppc : sort -> attribute list -> unit = fun s attr_l ->
  print ppc "symbol %s : typeK;" (pp s) ; pp_kore_attribute_list ppc attr_l

let pp_kore_hooked_sort ppc : sort -> attribute list -> unit =
  fun s attr_l ->
  print ppc "symbol %s : typeK;" (pp s) ;  pp_kore_attribute_list ppc attr_l

let pp_kore_symbol ppc : string -> symbol -> attribute list -> unit =
  fun keyword (name, qv_l, p_l, p) attr_l ->
  let prints = print ppc "%s" in
  prints keyword ; prints " " ; prints (pp name) ; prints " " ;
  pp_kore_quant_var_list ppc qv_l ;
  pp_kore_param_list ppc p_l ;
  print ppc "" ; pp_kore_param ppc p ; prints "; " ;
  pp_kore_attribute_list ppc attr_l

let rec pp_pattern_app ppc : axiom -> axiom StrMap.t -> unit = fun ax acc ->
  let prints = print ppc "%s" in
  match ax with
  | Dom_val(sort, n) ->
     print ppc "(#DV %s (%s))" (pp sort) (pp n)
  | Ceil(p_l, ax) ->
     prints "(#CEIL " ; pp_kore_param_list ppc p_l ;
     pp_pattern_app ppc ax acc ; pp_paren ppc
  | Predicate p -> pp_pattern_predicat ppc p acc
  | Equals(_, _, _) -> failwith "equals"
  | Exists(_, _, _) -> failwith "exists"
  | And(_, _, _) -> failwith "and"
  | Or(_, _, _) -> failwith "or"
  | Not(_, _) -> failwith "not"
  | Implies(_, _, _) -> failwith "implies"
  | Bottom _ -> failwith "bottom"
  | Top _ -> failwith "top"
  | Rewrites(_, _, _) -> failwith "rewrites"
  | In _ -> print ppc "HOP"
 (* | _ -> failwith "toto" *)
and pp_pattern_predicat ppc p acc = match p with
  | Sym(n, p_l, ax_l) ->
     print ppc "(%s " (pp n) ; pp_kore_param_list_bis ppc p_l ;
     let f ppc ax = pp_pattern_app ppc ax acc in
     (match ax_l with
      | [] -> ()
      | _  -> pp_list ppc "" f ax_l " " "") ; pp_paren ppc
  | Var(n, _) ->
    (match StrMap.find_opt n acc with
     | None   -> print ppc "%s " (pp n) (* ; pp_kore_param ppc p *)
     | Some x -> pp_pattern_app ppc x acc) (* ; pp_kore_param ppc p *)

let rec pp_kore_axiom ppc : int -> axiom -> axiom StrMap.t -> unit = fun step ax acc ->
  let prints = print ppc "%s" in
  let tmp : param list -> int -> axiom -> axiom -> unit =
    fun p_l step ax1 ax2 ->
    pp_kore_param_list_bis ppc p_l ;
    pp_endline ppc ;
    alignment ppc step ; pp_kore_axiom ppc (step+1) ax1 acc ;
    prints "\n" ;
    alignment ppc step ; pp_kore_axiom ppc (step+1) ax2 acc
  in
  let tmp2 : param list -> name -> param -> int -> axiom -> unit =
    fun p_l n p step ax ->
    pp_kore_param ppc p ; print ppc " " ; pp_kore_param_list_bis ppc p_l ;
    pp_endline ppc ;
    alignment ppc step ; print ppc "(λ %s : iK " (pp n) ; pp_kore_param ppc p ;
    print ppc ", %s" "\n" ;
    alignment ppc step ; pp_kore_axiom ppc (step+1) ax acc
  in
  match ax with
  | Equals(p_l, ax1, ax2) ->
     prints "(#EQUALS " ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Exists(p_l, (n,p), ax) ->
     prints "(#EXISTS " ; tmp2 p_l n p step ax ; pp_paren ppc ; pp_paren ppc
  | And(p_l, ax1, ax2) ->
     prints "(#AND " ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Or(p_l, ax1, ax2) ->
     prints "(#OR " ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Not(p_l, ax) ->
     prints "(#NOT " ; pp_kore_param_list_bis ppc p_l ;
     pp_endline ppc ; alignment ppc step ;
     pp_kore_axiom ppc (step+1) ax acc ; pp_paren ppc
  | Implies(p_l, _, ax2) ->
     prints "(#IMPLIES " ; tmp p_l step (Top p_l) ax2 ; pp_paren ppc
  | Bottom p_l ->
     prints "(#BOTTOM " ; pp_kore_param_list_bis ppc p_l ; pp_paren ppc
  | Top p_l ->
     prints "(#TOP " ; pp_kore_param_list_bis ppc p_l ; pp_paren ppc
  | Rewrites(p_l, ax1, ax2) ->
     prints "(#REWRITES " ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | In(p_l, (n,p), ax) ->
     prints "(#IN " ; tmp2 p_l n p step ax ; pp_paren ppc
  | Dom_val(sort, n) ->
     print ppc "(#DV %s (%s))" (pp sort) (pp n)
  | Ceil(p_l, ax) ->
     prints "(#CEIL " ; pp_kore_param_list_bis ppc p_l ;
     pp_endline ppc ; alignment ppc step ;
     pp_kore_axiom ppc (step+1) ax acc ; pp_paren ppc
  | Predicate p -> if !verbose then pp_kore_predicat_verbose ppc step p acc
                   else pp_kore_predicat ppc step p acc
and pp_kore_predicat_verbose ppc step p acc = match p with
  | Sym(n, p_l, ax_l) ->
     print ppc "#SYM(%s" (pp n) ; pp_kore_param_list_bis ppc p_l ;
     let f ppc ax =
       pp_endline ppc ; alignment ppc step ; pp_kore_axiom ppc (step+1) ax acc
     in
     (match ax_l with
      | [] -> ()
      | _  -> pp_list ppc "(" f ax_l "," ")" ; pp_paren ppc)
  | Var(n, p) ->
    print ppc "#VAR(%s : " (pp n) ; pp_kore_param ppc p ; pp_paren ppc
and pp_kore_predicat ppc step p acc = match p with
  | Sym(n, p_l, ax_l) ->
     print ppc "(%s " (pp n) ; pp_kore_param_list_bis ppc p_l ;
     let f ppc ax =
       pp_endline ppc ; alignment ppc step ; pp_kore_axiom ppc (step+1) ax acc
     in
     (match ax_l with
      | [] -> ()
      | _  -> pp_list ppc "" f ax_l " " "") ; pp_paren ppc
  | Var(n, _) ->
    (match StrMap.find_opt n acc with
     | None   -> print ppc "%s " (pp n) (* ; pp_kore_param ppc p *)
     | Some x -> pp_pattern_app ppc x acc) (* ; pp_kore_param ppc p *)

let pp_kore_def ppc : def -> unit = fun def ->
  match def with
  | A ax     -> pp_endline ppc ; space ppc ; pp_kore_axiom ppc 2 ax StrMap.empty
  | D(n, qv) -> print ppc "%s : %s" (pp n) (pp qv)

let pp_kore_alias ppc : alias -> attribute list -> unit =
  fun (sym, (n, qv_l, p_l, def)) attr_l ->
  pp_kore_symbol ppc "alias" sym attr_l ; pp_endline ppc ;
  print ppc "where %s " (pp n) ;
  pp_kore_quant_var_list ppc qv_l ;
  let f ppc (n,p) = print ppc "%s : " (pp n) ; pp_kore_param ppc p in
  pp_list ppc "(" f p_l ", " ") :=" ;
  pp_kore_def ppc def ;
  pp_kore_attribute_list ppc attr_l

let pp_kore_import ppc : import -> unit = fun (n, attr_l) ->
  print ppc "import %s " (pp n) ; pp_kore_attribute_list ppc attr_l

let pp_sem_kommand ppc cd : kommand list -> unit = fun kommand_l ->
  let f_sort (attr_l, _) _ _ s = pp_kore_sort ppc s attr_l, () in
  let f_symbol keyword (attr_l, _) _ _ sym =
    pp_kore_symbol ppc keyword sym attr_l, ()
  in
  let data_collect ax =
    let rec aux ax ((acc1, acc2) as acc) = match ax with
      | In(_, (n,_), Predicate(Var(x,y))) -> StrMap.add x (Predicate(Var(n,y))) acc1, StrMap.add x false acc2
      | In(_, (n,_), (Predicate(Sym(_,_,_)) as ax)) -> StrMap.add n ax acc1, StrMap.add n false acc2
      | In(_, (n,_), (Dom_val(_,_) as ax)) -> StrMap.add n ax acc1, StrMap.add n false acc2
      | In(_, (n,_), _) -> StrMap.add n ax acc1, StrMap.add n false acc2
      | Equals(_, ax1, ax2) -> aux ax2 (aux ax1 acc)
      | Exists(_, x, ax) -> aux ax  (acc1, StrMap.add (fst x) false acc2)
      | And(_, ax1, ax2) -> aux ax2 (aux ax1 acc)
      | Or(_,  _, _) -> acc (* aux ax2 (aux ax1 acc) *)
      | Not(_, _) -> acc (* aux ax acc *)
      | Implies(_, ax1, ax2) -> aux ax2 (aux ax1 acc)
      | Bottom _ -> acc
      | Top _ -> acc
      | Rewrites(_, ax1, ax2) -> aux ax2 (aux ax1 acc)
      | Dom_val _ -> acc
      | Ceil(_, ax) -> aux ax acc
      | Predicate (Sym(_, _, ax_l)) ->
        List.fold_right (fun ax acc -> aux ax acc) ax_l acc
      | Predicate (Var(n,_)) -> acc1, if not(StrMap.mem n acc2) then StrMap.add n true acc2 else acc2
    in
    let tmp1, tmp2 = aux ax (StrMap.empty, StrMap.empty) in
    let rec get_var_ax acc ax = match ax with
      | Predicate (Sym(_, _, ax_l)) -> List.fold_left get_var_ax acc ax_l
      (* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
      | Dom_val _ -> acc
      | Predicate (Var(x,_)) -> StrMap.add x true acc
      | Equals(_, ax1, ax2) | And(_, ax1, ax2) | Or(_, ax1, ax2)
      | Implies(_, ax1, ax2) | Rewrites(_, ax1, ax2) -> get_var_ax (get_var_ax acc ax1) ax2
      | Exists(_, _, ax) | Not(_, ax) | Ceil(_, ax) -> get_var_ax acc ax
      | Bottom _ | Top _ -> acc
      | In(_, _, ax) -> get_var_ax acc ax
    in
    tmp1, StrMap.fold (fun _ v acc -> get_var_ax acc v) tmp1 tmp2
    (* fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
  in
  let get_return_type ax : string =
    let tmp = match ax with
      | Equals(p_l, _, _) | Exists(p_l, _, _) | And(p_l, _, _)
      | Or(p_l, _, _) | Not(p_l, _) | Implies(p_l, _, _)
      | Bottom p_l | Top p_l | Rewrites(p_l, _, _) | In(p_l, _, _)
      | Ceil(p_l, _) | Predicate (Sym(_, p_l, _)) -> List.nth p_l ((List.length p_l) - 1)
      | Dom_val(sort, _) -> S sort
      | Predicate (Var(_,x)) -> x
    in
    match tmp with S x | Q x -> x
  in
  let f_axiom :
        data -> unit -> unit -> quant_var list * axiom
        -> unit * unit =
    fun (attr_l, _) _ _ (qv_l, ax) ->
    let data_map = data_collect ax in
    fresh_name_ax ppc () ; pp_kore_quant_var_list ppc qv_l ;
    StrMap.iter (fun key b -> if b then print ppc "%s " key) (snd data_map) ;
    print ppc ": d %s\n" (pp (get_return_type ax)) ; space ppc ; pp_kore_axiom ppc 2 ax (fst data_map) ; print ppc ";" ;
    pp_endline ppc ; pp_kore_attribute_list ppc attr_l ; pp_endline ppc, ()
  in
  let f_axiom_bis :
        data -> unit -> unit -> alias -> quant_var list * axiom
        -> unit * unit =
    fun (attr_l, _) _ _ (_, (n, _, _, def)) (qv_l, ax) -> match ax with Rewrites(p_l,Predicate(Sym(_,_,var_l)),rhs) ->
    print ppc "symbol %s " n ; pp_kore_quant_var_list ppc qv_l ;
    List.iter (fun x -> print ppc "%s " (match x with Predicate(Var(n,_)) -> pp n | _ -> "toto")) var_l ;
    print ppc ": d %s\n" (pp (get_return_type ax)) ;
    space ppc ; print ppc "%s" "(#REWRITES " ; pp_kore_param_list_bis ppc p_l ;
    space ppc ; space ppc ; pp_kore_def ppc def ; pp_endline ppc ;
    space ppc ; space ppc ; pp_kore_axiom ppc 4 rhs StrMap.empty ; pp_paren ppc ; print ppc ";" ;
    pp_endline ppc ; pp_kore_attribute_list ppc attr_l ; pp_endline ppc, ()

                                                                      | _ -> failwith "toto"
(*
alias -> attribute list -> unit =
  fun (sym, (n, qv_l, p_l, def)) attr_l ->
  pp_kore_symbol ppc "alias" sym attr_l ; pp_endline ppc ;
  print ppc "where %s " (pp n) ;
  pp_kore_quant_var_list ppc qv_l ;
  let f ppc (n,p) = print ppc "%s : " (pp n) ; pp_kore_param ppc p in
  pp_list ppc "(" f p_l ", " ") :=" ;
  pp_kore_def ppc def ;
  pp_kore_attribute_list ppc attr_l



      f_axiom data x () (qv_l, ax) *)
  in
  let res = kommand_iter_without_alias cd kommand_l () ()
            f_sort f_sort (f_symbol "symbol") (f_symbol "symbol")
            (*   (fun (attr_l, _) _ _ al -> pp_kore_alias ppc al attr_l, ()) *)
            (fun (_, _) _ _ _ -> (), ())
            (f_axiom_bis, f_axiom_bis, f_axiom_bis)
            f_axiom (f_axiom, f_axiom)
            (f_axiom, f_axiom, f_axiom, f_axiom, f_axiom) f_axiom f_axiom
            (f_axiom, f_axiom, f_axiom, f_axiom, f_axiom, f_axiom, f_axiom)
            f_axiom (* TODO claim ? *)
            (fun () -> pp_endline ppc)
  in
  fst res
