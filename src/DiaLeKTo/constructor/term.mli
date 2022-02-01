
open Type.Term
(* open Type.Command *)

(** Smart constructor *)

(** ************* *)
(** Term for type *)
(** ************* *)

(** [v_TYPE] is the constant type "Type". *)
val v_TYPE : term

(** [cr_appl_multi t t_l] corresponds to:
       - t if t_l = []
       - ((((t t1) t2) ...) tn) if t_l = [t1;...;tn] *)
val cr_appl_multi : term -> term list -> term

(** [cr_appl t1 t2] corresponds to (t1 t2). *)
val cr_appl      : term -> term -> term

(** [cr_var i] creates a variable named [i]. *)
val cr_var      : iden -> term

(** [cr_sym i] creates a symbol named [i]. *)
val cr_sym      : iden -> term

(** [cr_sym_appl i t_l] corresponds to:
       - i if t_l = []
       - ((((i t1) t2) ...) tn) if t_l = [t1;...;tn]. *)
val cr_sym_appl : iden ->  term list -> term


(** [cr_lambda ((v_l, t1), t2)] corresponds to:
       - t2 if v_l = []
       - λ(x1 ... xn : t1), t2 if v_l = [x1;...;xn]. *)
val cr_lambda : (iden list * term option) * term -> term

(** [cr_lambda (binder_l, t)] corresponds to:
       - t if binder_l = []
       - λ(x1 ... xn : tx), ..., λ(k1 ... km : tk), t
        if binder_l = [ ([x1;...;xn], tx); ...; ([k1;...;km], tk). *)
val cr_lambda_full : (iden list * term option) list * term -> term

(** [cr_lambda_expand ((v_l, t1), t2)] corresponds to:
       - t2 if v_l = []
       - λ(x1 : t1), ..., λ(xn : t1), t2 if v_l = [x1;...;xn]. *)
val cr_lambda_expand : (iden list * term option) * term -> term

(** [cr_lambda_full_expand (binder_l, t)] corresponds to:
       - t if binder_l = []
       - λ(x1 : tx), ..., λ(xn : tx), ...,
           λ(k1 : tk), ..., λ(km : tk), t
        if binder_l = [ ([x1;...;xn], tx); ...; ([k1;...;km], tk). *)
val cr_lambda_full_expand : (iden list * term option) list * term -> term


(** [cr_pi ((v_l, t1), t2)] corresponds to [cr_lambda ((v_l, t1), t2)]
    where λ is replaced by Π. *)
val cr_pi : (iden list * term option) * term -> term

(** [cr_pi (binder_l, t)] corresponds to [cr_lambda (binder_l, t)]
    where λ is replaced by Π. *)
val cr_pi_full : (iden list * term option) list * term -> term

(** [cr_pi_expand ((v_l, t1), t2)] corresponds to
    [cr_lambda_expand ((v_l, t1), t2)] where λ is replaced by Π. *)
val cr_pi_expand : (iden list * term option) * term -> term

(** [cr_pi_full_expand (binder_l, t)] corresponds to
    [cr_lambda_full_expand (binder_l, t)] where λ is replaced by Π. *)
val cr_pi_full_expand : (iden list * term option) list * term -> term


(** [cr_arrow t1 t2] creates the type t1 -> t2. *)
val cr_arrow  : term -> term -> term

(** [cr_arrow_appl t t_l] corresponds to:
       - t if t_l = []
       - t1 -> (... -> (tn -> t)) if t_l = [t1;...;tn]. *)
val cr_arrow_appl : term ->  term list -> term


(** *********************** *)
(** Term for rewriting rule *)
(** *********************** *)

val v_WILDCARD     : pattern
val cr_pattern_var : iden -> pattern
val cr_pattern     : iden * pattern list -> pattern

(** ********* *)
(** Utilities *)
(** ********* *)

(** [to_full_arrow t] replaces, when it is possible,
    [Π(x : t1), t2] by [t1 -> t2]. *)
val to_full_arrow : term -> term

(** [to_full_pi t] replaces all [t1 -> t2] by [Π(x : t1), t2],
    where x is fresh. The name of the variable x begins by [Var]. *)
val to_full_pi : term -> term

(* TODO fix
val from_symbol : sym_decl -> term *)
(*
(** [cr_application t l]
      - l = [] : return [t]
      - l = [x] : return the same thing as [cr_Appl]
      - l = [t1, ..., tn] return ((((t t1) t2) ...) tn) *)
val cr_application : term -> term list -> term

val cr_application_map : term -> (term -> term) -> term list -> term

(*
(** [add_args_map f t ts] is equivalent to [add_args t (List.map f ts)] but
   more efficient. *)
val add_args_map : term -> (term -> term) -> term list -> term
 *)


val cr_abstraction : iden list -> term -> term -> term



(* val cr_dep_product : var * term -> (var * term) list -> term -> term *)



val cr_non_dep_product : term list -> term -> term
                                              *)
