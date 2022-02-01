(* Pas besoin de garder les positions : calculer par LP ou DK *)

(*

type ('c, 'v) lpcmt_term =
  [ `Type
  | `Const  of 'c
  | `Var    of 'v
  | `Appl   of             ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term
  | `Lambda of 'v        * ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term
  | `Pi     of 'v option * ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term ]

type pattern =
  | Var      of loc * ident * int * pattern list (* Y x1 ... xn *)
  | Pattern  of loc * name * pattern list
  | Lambda   of loc * ident * pattern

type ('c, 'v) lhs_term =
  [ `Type
  | `Const  of 'c
  | `Var    of 'v
  | `Appl   of             ('c, 'v) lhs_term * ('c, 'v) lhs_term
  | `Lambda of 'v        * ('c, 'v) lhs_term * ('c, 'v) lhs_term
  | `Pi     of 'v option * ('c, 'v) lhs_term * ('c, 'v) lhs_term
  | `Joker ]

type ('c, 'v) lpcmt_term =
  | Type
  | Const  of 'c
  | Var    of 'v
  | Appl   of             ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term
  | Lambda of 'v        * ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term
  | Pi     of 'v option * ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term

type ('c, 'v) lpcmt_tterm =
  [ `Type
  | `Const  of 'c
  | `Var    of 'v
  | `Appl   of             ('c, 'v) lpcmt_tterm * ('c, 'v) lpcmt_tterm
  | `Lambda of 'v        * ('c, 'v) lpcmt_tterm * ('c, 'v) lpcmt_tterm
  | `Pi     of 'v option * ('c, 'v) lpcmt_tterm * ('c, 'v) lpcmt_tterm
  | `Arrow  of  ]


type ('c, 'v) lpcmt_sterm =
  [ `Type
  | `Const  of 'c
  | `Var    of 'v
  | `Appl   of ('c, 'v) lpcmt_sterm * ('c, 'v) lpcmt_sterm
               * (('c, 'v) lpcmt_sterm) list
  | `Lambda of
      'v        * (('c, 'v) lpcmt_sterm) option * ('c, 'v) lpcmt_sterm
  | `Pi     of
      'v option *  ('c, 'v) lpcmt_sterm         * ('c, 'v) lpcmt_sterm
  | `Arrow  of     ('c, 'v) lpcmt_sterm         * ('c, 'v) lpcmt_sterm ]

let toto x : ('c, 'v) lpcmt_sterm = match x with
  | `Appl (t1, t2) -> x
  | t -> t



type p_term = p_term_aux loc
and p_term_aux =
  | P_Type (** TYPE constant. *)
  | P_Iden of p_qident * bool (** Identifier. The boolean indicates whether
                                 the identifier is prefixed by "@". *)
  | P_Wild (** Underscore. *)
  | P_Meta of p_meta_ident * p_term array
    (** Meta-variable with explicit substitution. *)
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







               *)


type ('a , 'b) typeof = 'a list * 'b

let expand_typeof : ('a, 'b) typeof -> (('a, 'b) typeof) list =
  fun d ->
  let l, t = d in
  let f : 'a -> 'y = fun v -> ([v], t) in
  List.map f l

let test = expand_typeof (["v";"a";"c"], "term")

let simpl_typeof : (('a, 'b) typeof) list -> (('a, 'b) typeof)  list =
  fun d ->
  let rec aux x = match x with
    | []  -> []
    | [h] -> [h]
    | (l1,t1)::(l2,t2)::q ->
       if t1 = t2 then aux ((l1@l2, t1)::q)
       else (l1,t1)::(aux ((l2,t2)::q))
  in
  aux d

let res = simpl_typeof test




(*

type preterm =
  | PreType of loc

  | PreId  of loc * ident (* var *)
  | PreQId of loc * name  (* cst *)

  | PreApp of preterm * preterm * preterm list
  | PreLam of loc * ident * preterm option * preterm
  | PrePi  of loc * ident option * preterm * preterm

let rec pp_preterm fmt preterm =
  match preterm with
  | PreType _                -> fprintf fmt "Type"
  | PreId (_, v)             -> pp_ident fmt v
  | PreQId (_, cst)          -> fprintf fmt "%a" pp_name cst
  | PreApp (f, a, lst)       -> pp_list " " pp_preterm_wp fmt (f :: a :: lst)
  | PreLam (_, v, None, b)   -> fprintf fmt "%a => %a" pp_ident v pp_preterm b
  | PreLam (_, v, Some a, b) ->
      fprintf fmt "%a:%a => %a" pp_ident v pp_preterm_wp a pp_preterm b
  | PrePi (_, o, a, b)       -> (
      match o with
      | None   -> fprintf fmt "%a -> %a" pp_preterm_wp a pp_preterm b
      | Some v ->
          fprintf fmt "%a:%a -> %a" pp_ident v pp_preterm_wp a pp_preterm b)

and pp_preterm_wp fmt preterm =
  match preterm with
  | (PreType _ | PreId _ | PreQId _) as t -> pp_preterm fmt t
  | t -> fprintf fmt "(%a)" pp_preterm t

         *)

(* let to_typeof *)

type 'a lpm_term = [ `Kind
                | `Type of 'a
                | `Const
                | `Var
                | `Appl of 'a lpm_term * 'a lpm_term
                | `Lambda
                | `Prod
                | `Arrow ]

type iden = string
(* type 'a r_term = [ 'a lpm_term | `Patt of iden ] *)

type 'a r_term = [ `Kind
                | `Type of 'a
                | `Const
                | `Var
                | `Appl of 'a r_term * 'a r_term
                | `Lambda
                | `Prod
                | `Arrow
                | `Patt of iden
               ]

let test42 : 'a r_term = `Appl(`Kind, `Kind)
(*
type 'a rule = 'a r_term * 'a r_term

let get_var_of_rule : 'a rule -> iden list = fun (l,_) ->
  let rec aux (x : 'a r_term) acc = match x with
    | `Patt i -> if List.mem i acc then acc else i::acc
    | `Appl(t1,t2) -> (aux t1 [])@(aux t2 [])@acc
    | _ -> acc
  in
  aux l []

let tmp = get_var_of_rule (test42, test42)

let test_LHS : 'a r_term = `Appl(`Const, `Patt("x"))
let test_RHS : 'a r_term = `Patt("x")

let _ = get_var_of_rule (test_LHS, test_RHS)
*)

type 'a lp_term = [ `Kind
                | `Type
                | `Const
                | `Var
                | `Appl of 'a lpm_term * 'a lpm_term
                | `Lambda
                | `Prod
                | `Arrow
                | `LLet ]

let toto : 'a lp_term = `Kind

let titi : ('a option) lpm_term = `Appl(`Type None, `Const)

let typ = `Type None

let f = function
  | `Kind -> true
  | _ -> false

let r = f toto

let r2 = f titi

type term = string
