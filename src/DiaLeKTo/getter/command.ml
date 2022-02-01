(* A priori, pas besoin de signature pour un traducteur (ou au moins pour
   le mien...)



   (** [add_external_declaration sg l cst sc st ty] declares the symbol [id] of type
    [ty], scope [sc] and staticity [st] in the environment [sg]. *)
val add_external_declaration :
  t -> name -> scope -> staticity -> term -> unit

(** [add_declaration sg l id sc st ty] declares the symbol [id] of type [ty]
    and staticity [st] in the environment [sg].
    If [sc] is [Private] then the symbol cannot be used in other modules *)
val add_declaration : t -> ident -> scope -> staticity -> term -> unit

(** [add_rules sg rule_lst] adds a list of rule to a symbol in the environement [sg].
    All rules must be on the same symbol. *)
val add_rules : t -> Rule.rule_infos list -> unit



   Lors de ma traduction, je n'ai besoin que des informations de l'entité
   que j'ai sous les yeux. *)

open Type.Term
open Type.Command

(** ********************* *)
(**  Logic statement      *)
(** ********************* *)

let get_assoc_identity : sym_identity -> associativity option = fun s ->
  match s.parsing.mixfix with
  | Infix a -> Some a
  | _ -> None

let get_prec_identity : sym_identity -> precedence = fun s ->
  s.parsing.prec

let get_mixfix_identity : sym_identity -> mixfix = fun s ->
  s.parsing.mixfix

let get_parsing_rule_identity : sym_identity -> parsing_rule = fun s ->
  s.parsing

let get_visibility_identity : sym_identity -> visibility = fun s ->
  s.visibility

let get_algebra_identity : sym_identity -> algebra = fun s ->
  match s.prop with
  | Definable a -> a
  | _ -> Free

let get_property_identity : sym_identity -> property = fun s -> s.prop

let get_name_identity : sym_identity -> iden = fun s -> s.name

(** ***************** *)
(** A. Symbol         *)
(** ***************** *)

(** About the parsing of the symbol *)

let get_sym_assoc : sym_decl -> associativity option = fun s ->
  get_assoc_identity s.sym

let get_sym_prec : sym_decl -> precedence = fun s -> get_prec_identity s.sym

let get_sym_mixfix : sym_decl -> mixfix = fun s -> get_mixfix_identity s.sym

let get_sym_parsing_rule : sym_decl -> parsing_rule = fun s ->
  get_parsing_rule_identity s.sym

(** About the visibility of the symbol *)

let get_sym_visibility : sym_decl -> visibility = fun s ->
  get_visibility_identity s.sym

(** About the property of the symbol *)

let get_sym_algebra : sym_decl -> algebra = fun s ->
  get_algebra_identity s.sym

let get_sym_property : sym_decl -> property = fun s ->
  get_property_identity s.sym

let get_sym_name : sym_decl -> iden = fun s -> get_name_identity s.sym

(** About the type of the symbol *)

let is_implicit : param -> bool = fun p ->
  match p with
  | Impl _ -> true
  | Expl _ -> false

(** filter f l returns all the elements of the list l that satisfy the predicate f.
    The order of the elements in the input list is preserved. *)
let get_sym_implicit_var : sym_decl -> param list = fun s ->
  List.filter is_implicit (fst s.typ)
  (*  let rec aux l acc = match l with
    | []   -> acc
    | h::q -> if is_implicit h then aux q (h::acc) else aux q acc
  in
  aux (fst s.typ) [] *)

let is_explicit : param -> bool = fun p -> not (is_implicit p)

let get_sym_explicit_var : sym_decl -> param list = fun s ->
 List.filter is_explicit (fst s.typ)

let split_sym_var : sym_decl -> param list * param list = fun s ->
  List.partition is_explicit (fst s.typ)

let get_sym_type : sym_decl -> term = fun s -> snd s.typ
(* TODO take in count the parameters *)

(*
(** [get_neutral sg l md id] returns the neutral element of the ACU symbol
    [md.id]. *)
let get_neutral : name -> term = fun _ -> Type

(** [get_dtree sg filter l cst] returns the decision/matching tree
    associated with [cst] inside the environment [sg]. *)
(* val get_dtree : name -> Dtree.t *)

(** [get_rules sg lc cst] returns a list of rules that defines the
    symbol. *)
(* val get_rules : name -> rule_infos list *)
 *)

(** ***************** *)
(** B. Definition     *)
(** ***************** *)

(** About the parsing of the definition *)

let get_def_assoc : def_decl -> associativity option = fun d ->
  get_assoc_identity d.sym

let get_def_prec : def_decl -> precedence = fun d -> get_prec_identity d.sym

let get_def_mixfix : def_decl -> mixfix = fun d -> get_mixfix_identity d.sym

let get_def_parsing_rule : def_decl -> parsing_rule = fun d ->
  get_parsing_rule_identity d.sym

(** About the visibility of the definition *)

let get_def_visibility : def_decl -> visibility = fun d ->
  get_visibility_identity d.sym

(** About the property of the definition *)

let get_def_algebra : def_decl -> algebra = fun d ->
  get_algebra_identity d.sym

let get_def_property : def_decl -> property = fun d ->
  get_property_identity d.sym

let get_def_name : def_decl -> iden = fun d -> get_name_identity d.sym

(** About the type of the definition *)

let get_def_implicit_var : def_decl -> param list = fun d ->
  match d.typ with
  | None   -> failwith "I don't have the type of the definition"
  | Some t -> List.filter is_implicit (fst t)

let get_def_explicit_var : def_decl -> param list = fun d ->
  match d.typ with
  | None   -> failwith "I don't have the type of the definition"
  | Some t -> List.filter is_explicit (fst t)

let split_def_var : def_decl -> param list * param list = fun d ->
  match d.typ with
  | None   -> failwith "I don't have the type of the definition"
  | Some t -> List.partition is_explicit (fst t)

let get_def_type : def_decl -> term option = fun d ->
  match d.typ with
  | None   -> None
  | Some t -> Some (snd t)
(* TODO take in count the parameters *)

(** About the body of the definition *)

let get_lambda_term : def_decl -> term option = fun d ->
  match d.def with
  | LambdaTerm t -> Some t
  | Script _     -> None

let is_opaque : def_decl -> bool = fun d -> d.opacity

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

(* let list_rv_all x l =
  let rec aux = function
    | [] -> []
    | h::q -> if h = x then aux q else h::(aux q)
  in
  aux l


(** [get_rule_var r] returns (var in LHS, var in RHS, var not usued) *)
let get_rule_var : rule -> iden list * int * iden list = fun r ->
  let lhs, rhs = r in
  let rec aux_lhs (var_lhs, nb_var_unusued) lhs = match lhs with
    | Wildcard -> var_lhs, nb_var_unusued + 1
    | Var v -> v::var_lhs, nb_var_unusued
    | Pattern(_, patt_l) ->
       List.fold_left aux_lhs (var_lhs, nb_var_unusued) patt_l
  in
  let rec aux_rhs rhs acc = match rhs with
    | Type  -> acc
    | Var v -> v::acc
    | Sym _ -> acc
    | Appl(t1, t2) -> aux_rhs t2 (aux_rhs t1 acc)
    | Lambda((v, _ (* TODO *)), t) ->
       let tmp = aux_rhs t acc in list_rv_all v tmp
    | Pi    ((v, _ (* TODO *)), t) ->
       let tmp = aux_rhs t acc in list_rv_all v tmp
  in
  let tmp_lhs, tmp_unusued = aux_lhs ([], 0) lhs in
  (tmp_lhs, tmp_unusued, aux_rhs rhs [])
 *)

(** ********************* *)
(**  Set option           *)
(** ********************* *)

(* TODO *)

(** ********************* *)
(**  Query                *)
(** ********************* *)

(* TODO *)

(** ********************* *)
(**  (Un)safe command     *)
(** ********************* *)

(* TODO *)
