
open Type.Term
(* open Type.Command *)

(** Smart constructor *)

(** Term for type *)

(* (* let v_Kind : term = `Kind     allow ? *)
   let v_TYPE : term = `Type

   (* TODO mettre iden partout ??? *)
   let cr_Var  : var  -> term = fun v -> `Var v
   let cr_Symb : iden -> term = fun i -> `Sym i
   let cr_Appl : term -> term -> term = fun t1 t2 -> `Appl(t1, t2)
   let cr_Abst : (var * term option * term) -> term = fun x -> `Abst x
   let cr_Prod : (var * term option * term) -> term = fun x -> `Prod x *)

(** [v_TYPE] is the constant type "Type". *)
let v_TYPE : term = Type

(** [cr_appl_multi t t_l] corresponds to:
       - t if t_l = []
       - ((((t t1) t2) ...) tn) if t_l = [t1;...;tn] *)
let cr_appl_multi : term -> term list -> term = fun t t_l -> Appl(t, t_l)

(** [cr_appl t1 t2] corresponds to (t1 t2). *)
let cr_appl      : term -> term -> term = fun t1 t2 -> cr_appl_multi t1 [t2]

(** [cr_var i] creates a variable named [i]. *)
let cr_var      : iden -> term = fun v -> Var v

(** [cr_sym i] creates a symbol named [i]. *)
let cr_sym      : iden -> term = fun i -> Sym i

(** [cr_sym_appl i t_l] corresponds to:
       - i if t_l = []
       - ((((i t1) t2) ...) tn) if t_l = [t1;...;tn]. *)
let cr_sym_appl : iden ->  term list -> term = fun i t_l ->
  cr_appl_multi (cr_sym i) t_l


(** Some meta-functions *)

type meta_b = (iden list * term option) * term -> term

(** [cr_meta_binder meta_b ((v_l, t1), t2)] corresponds to:
       - t2 if v_l = []
       - meta_b (x1 ... xn : t1), t2 if v_l = [x1;...;xn]. *)
let cr_meta_binder : meta_b -> (iden list * term option) * term -> term =
  fun meta_b ((v_l, t1), t2) -> meta_b ((v_l, t1), t2)

(** [cr_meta_binder_full meta_b (binder_l, t)] corresponds to:
       - t if binder_l = []
       - meta_b (x1 ... xn : tx), ..., meta_b (k1 ... km : tk), t
        if binder_l = [ ([x1;...;xn], tx); ...; ([k1;...;km], tk). *)
let cr_meta_binder_full :
      meta_b -> (iden list * term option) list * term -> term =
  fun meta_b (binder_l, t) ->
  List.fold_right (fun a b -> cr_meta_binder meta_b (a, b)) binder_l t

(** [cr_meta_binder_expand meta_b ((v_l, t1), t2)] corresponds to:
       - t2 if v_l = []
       - meta_b (x1 : t1), ..., meta_b (xn : t1), t2 if v_l = [x1;...;xn]. *)
let cr_meta_binder_expand :
      meta_b -> (iden list * term option) * term -> term =
  fun meta_b ((v_l, t1), t2) ->
  List.fold_right (fun a b -> cr_meta_binder meta_b (([a], t1), b)) v_l t2

(** [cr_meta_binder_full_expand meta_b (binder_l, t)] corresponds to:
       - t if binder_l = []
       - meta_b (x1 : tx), ..., meta_b (xn : tx), ...,
           meta_b (k1 : tk), ..., meta_b (km : tk), t
        if binder_l = [ ([x1;...;xn], tx); ...; ([k1;...;km], tk). *)
let cr_meta_binder_full_expand :
      meta_b -> (iden list * term option) list * term -> term =
  fun meta_b (binder_l, t) ->

  let f =
    fun a b -> cr_meta_binder meta_b (a, b)
  in
  List.fold_right f binder_l t





let id_Lambda = fun (a, b) -> Lambda(a, b)
let id_Pi = fun (a, b) -> Pi(a, b)

(** [cr_lambda ((v_l, t1), t2)] corresponds to:
       - t2 if v_l = []
       - λ(x1 ... xn : t1), t2 if v_l = [x1;...;xn]. *)
let cr_lambda : (iden list * term option) * term -> term =
  fun ((v_l, t1), t2) ->
  cr_meta_binder id_Lambda ((v_l, t1), t2)

(** [cr_lambda (binder_l, t)] corresponds to:
       - t if binder_l = []
       - λ(x1 ... xn : tx), ..., λ(k1 ... km : tk), t
        if binder_l = [ ([x1;...;xn], tx); ...; ([k1;...;km], tk). *)
let cr_lambda_full : (iden list * term option) list * term -> term =
  fun (binder_l, t) -> cr_meta_binder_full id_Lambda (binder_l, t)

(** [cr_lambda_expand ((v_l, t1), t2)] corresponds to:
       - t2 if v_l = []
       - λ(x1 : t1), ..., λ(xn : t1), t2 if v_l = [x1;...;xn]. *)
let cr_lambda_expand : (iden list * term option) * term -> term =
  fun ((v_l, t1), t2) -> cr_meta_binder_expand id_Lambda ((v_l, t1), t2)

(** [cr_lambda_full_expand (binder_l, t)] corresponds to:
       - t if binder_l = []
       - λ(x1 : tx), ..., λ(xn : tx), ...,
           λ(k1 : tk), ..., λ(km : tk), t
        if binder_l = [ ([x1;...;xn], tx); ...; ([k1;...;km], tk). *)
let cr_lambda_full_expand : (iden list * term option) list * term -> term =
  fun (binder_l, t) -> cr_meta_binder_full_expand id_Lambda (binder_l, t)


(** [cr_pi ((v_l, t1), t2)] corresponds to [cr_lambda ((v_l, t1), t2)]
    where λ is replaced by Π. *)
let cr_pi : (iden list * term option) * term -> term =
  fun ((v_l, t1), t2) -> cr_meta_binder id_Pi ((v_l, t1), t2)

(** [cr_pi (binder_l, t)] corresponds to [cr_lambda (binder_l, t)]
    where λ is replaced by Π. *)
let cr_pi_full : (iden list * term option) list * term -> term =
  fun (binder_l, t) -> cr_meta_binder_full id_Pi (binder_l, t)

(** [cr_pi_expand ((v_l, t1), t2)] corresponds to
    [cr_lambda_expand ((v_l, t1), t2)] where λ is replaced by Π. *)
let cr_pi_expand : (iden list * term option) * term -> term =
  fun ((v_l, t1), t2) -> cr_meta_binder_expand id_Pi ((v_l, t1), t2)

(** [cr_pi_full_expand (binder_l, t)] corresponds to
    [cr_lambda_full_expand (binder_l, t)] where λ is replaced by Π. *)
let cr_pi_full_expand : (iden list * term option) list * term -> term =
  fun (binder_l, t) -> cr_meta_binder_full_expand id_Pi (binder_l, t)


(** [cr_arrow t1 t2] creates the type t1 -> t2. *)
let cr_arrow  : term -> term -> term = fun t1 t2 -> Arrow(t1, t2)

(** [cr_arrow_appl t t_l] corresponds to:
       - t if t_l = []
       - t1 -> (... -> (tn -> t)) if t_l = [t1;...;tn]. *)
let cr_arrow_appl : term ->  term list -> term = fun t t_l ->
  List.fold_right cr_arrow t_l t


(** Term for rewriting rule *)

let v_WILDCARD     : pattern = Wildcard
let cr_pattern_var : iden -> pattern = fun v -> Var v
let cr_pattern     : iden * pattern list -> pattern =
  fun (i, p_l) -> Pattern(i, p_l)


(** Utilities *)

(** [to_full_arrow t] replaces, when it is possible,
    [Π(x : t1), t2] by [t1 -> t2]. *)
let to_full_arrow : term -> term = fun t -> t (* TODO *)

(** [to_full_pi t] replaces all [t1 -> t2] by [Π(x : t1), t2],
    where x is fresh. The name of the variable x begins by [VarNB]. *)
let to_full_pi : term -> term = fun t ->
  let nb = ref 0 in
  let rec aux t = match t with
    | Type | Sym _ | Var _ -> t

    | Lambda((v_l, None), t2) -> Lambda((v_l, None), aux t2)
    | Pi((v_l, None), t2) -> Pi((v_l, None), aux t2)

    | Lambda((v_l, Some t1), t2) -> Lambda((v_l, Some (aux t1)), aux t2)
    | Pi((v_l, Some t1), t2) -> Pi((v_l, Some (aux t1)), aux t2)

    | Appl(t, t_l) -> Appl(aux t, List.map aux t_l)
    | Arrow(t1, t2) -> incr nb ;
       Pi((["Var" ^ string_of_int !nb], Some t1), aux t2)
  in
  aux t

(*
(* TODO fix
let from_symbol : sym_decl -> term = fun s -> `Sym s.sym.name *)

(** [cr_application t l]
      - l = [] : return [t]
      - l = [x] : return the same thing as [cr_Appl]
      - l = [t1, ..., tn] return ((((t t1) t2) ...) tn) *)
(* let cr_application : term -> term list -> term = fun t t_l ->
  let rec aux l acc = match l with
    | []   -> acc
    | h::q -> aux q (Appl(acc, h))
  in
  aux t_l t *)
let cr_application : term -> term list -> term = fun t t_l ->
  Appl(t, t_l)

(** [cr_application_map t f t_l] is equivalent to
    [cr_application t (List.map f ts)] but more efficient. *)
(* let cr_application_map : term -> (term -> term) -> term list -> term =
  fun t f t_l ->
  let rec aux l acc = match l with
    | []   -> acc
    | h::q -> aux q (cr_Appl acc (f h))
  in
  aux t_l (f t) *)
let cr_application_map : term -> (term -> term) -> term list -> term =
  fun t f t_l -> Appl(t, List.map f t_l)

(** [cr_abstraction v_l t1 t2] returns \lambda (v1 ... vn : t1), t2
    where v_l = [v1;...;vn], and n = List.length v_l.
    If v_l = [], it returns t2 only. *)
(* let cr_abstraction : var list -> term -> term -> term = fun v_l t1 t2 ->
  let rec aux l acc = match l with
    | []   -> acc
    | v::q -> cr_Lambda (v, Some t1, aux q acc)
  in
  aux v_l t2 *)
let cr_abstraction : iden list -> term -> term -> term = fun v_l t1 t2 ->
  Lambda((v_l, Some t1), t2)

(** [cr_dep_product (v,t1) d_l t2] returns \Pi (v v1 ... vn : t1), t2
    where v_l = [v1;...;vn], and n = List.length v_l.
    If v_l = [], it returns t2 only. *)
(* let cr_dep_product : var * term -> (var * term) list -> term -> term =
  fun (v0,t0) d_l t ->
  let rec aux l acc = match l with
    | []   -> acc
    | (v1,t1)::q -> cr_Pi (v1, Some t1, aux q acc)
  in
  aux d_l (cr_Pi (v0, Some t0, t)) *)
(* TODO *let cr_dep_product :
      var list * term -> (var list * term) list -> term -> term =
  fun (v0,t0) d_l t ->
  List.fold_left (fun a b -> Pi(a, b)) t d_l *)

(* let cr_non_dep_product : term list -> term -> term = fun t_l t ->
  let rec aux l acc = match l with
    | []   -> acc
    | h::q -> aux q (cr_Arrow h acc)
  in
  aux t_l t
 *)
let cr_non_dep_product : term list -> term -> term = fun t_l t ->
  List.fold_right (fun a b -> Arrow(a, b)) t_l t



 *)


(* TODO
type ('c, 'v) lpcmt_term =
  [ `Type
  | `Const  of 'c
  | `Var    of 'v
  | `Appl   of             ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term
  | `Lambda of 'v        * ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term
  | `Pi     of 'v option * ('c, 'v) lpcmt_term * ('c, 'v) lpcmt_term ]

let v_TYPE : term = `Type

let cr_application : term -> term list -> term =
let cr_abstraction : `Var list -> term -> term =
let cr_dep_product : `Var * term -> (`Var * term) list -> term =
let cr_non_dep_product : term list -> term =

let v_WILDCARD : term ? = "_"


type name
type path

(** [create_path s_l] creates a path thanks to a list of string.
    Note: The option -r can't change the name. *)
let create_path : string list -> path = fun x -> x

(** [create_qident (p, s)] creates a qualified identifier thanks to
    a path [p] and a name [s]. Note: The option -r can change the name. *)
let create_qident : path * string -> qident = fun (p, s) -> (p, pp s)


(** [create_ident s] creates an identifier.
    Note: The option -r can change the name. *)
let create_ident : string -> ident = fun s -> pp s

(** [create_meta_var s] creates a meta-variable thanks to a name.
    Note: The option -r can't change the name. *)
let create_meta_var : string -> p_meta_ident = fun s -> no_pos (Name s)

(** P_term *)


(** [create_ident s] creates an identifier (just a name) without position.
    This identifier is not prefixed by "@", and can be changed by the option -r. *)
let create_ident : string -> p_term = fun s ->
  let tmp = create_p_qident ([], s) in
  no_pos (P_Iden(tmp, false))

(** [p_WILD] is the constant "_" without position. *)
let p_WILD : p_term = no_pos P_Wild

(** [create_meta s] creates a meta-variable without argument and position.
    Note: The option -r can't change the name. *)
let create_meta : string -> p_term = fun s ->
  no_pos (P_Meta (create_meta_var s, None))

(** [create_pattern_var s] creates a variable of pattern without argument and
    position. Note: The option -r can change the name. *)
let create_pattern_var : string -> p_term = fun s ->
  let name = Some (create_p_ident s) in
  no_pos (P_Patt(name, None))

(** [create_appl a1 a2] creates the application of [a1] on [a2], without
    position. *)
let create_appl : p_term -> p_term -> p_term = fun a1 a2 ->
  no_pos (P_Appl(a1, a2))

let create_one_arg sym v =
  create_appl (create_ident sym) (create_pattern_var v)

(** [create_arrow t1 t2] creates the type [t1 -> t2], without position. *)
let create_arrow : p_term -> p_term -> p_term = fun t1 t2 ->
  no_pos (P_Arro(t1, t2))

(** [create_implicit_arg s] creates an identifier (just a name) as
    implicit argument, without position. *)
let create_implicit_arg : string -> p_term = fun s ->
  no_pos (P_Expl(create_ident s))
(* P_Expl -> P_Impl? @TODO *)



(** [create_builtin_command opt sym] creates a builtin command,
    i.e. builtin [opt] := [sym]. *)
let create_builtin_command : string -> path * string -> p_command = fun opt sym ->
  no_pos (P_builtin(opt, (create_p_qident sym)))
  *)
