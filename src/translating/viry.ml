
(** This file generates the variante of Viry's transformation from
    Conditional Term Rewriting System (CTRS) to Term Rewriting System (TRS).
    Notre outil effectue deux traductions de CTRS vers TRS, selon que la
    condition est formée avec le prédicat isKResult, traduction effectuée
    dans le fichier eval_strategy.ml, ou un autre prédicat, traduction
    effectuée ici. *)

(** Encodage sur un exemple
    -----------------------

    Considérons le système suivant :
      (1) rule max X Y => Y requires X <Int Y
      (2) rule max X Y => X requires X >=Int Y
    L'encodage permet d'obtenir le TRS suivant dans Dedukti :
      (0)   rule max  $x $y          ↪ ♭max $x $y ♭ ♭
      (1')  rule ♭max $x $y ♭ $c     ↪ ♭max $x $y ($x < $y) $c
      (1'') rule ♭max $x $y true $c  ↪ $y
      (2')  rule ♭max $x $y $c ♭     ↪ ♭max $x $y $c ($x >= $y)
      (2'') rule ♭max $x $y $c true  ↪ $x

    L'idée générale de l'encodage, inspiré de la proposition de Viry,
    est d'ajouter, pour un symbole défini avec des règles conditionnelles,
    autant d'arguments qu'il y a de conditions.

    La règle (0) réécrit un terme dont le symbole de tête est max, par un
    terme utilisant la version étendue correspondante d'arité 4, ♭max, où
    tous les arguments booléens valent ♭, indiquant que les arguments
    booléens n'ont pas encore été initialisés par une condition.

    Les règles (1') et (2') initialisent les conditions à calculer,
    tandis que les règles (1'') et (2'') réduisent la taille du terme
    puisque une des conditions a été évaluée à true. *)

(** Formalisation de l'encodage
    ---------------------------

 Hypothèses :
   - Pour éviter des conflits de nommage, nous supposons que ♭ est un nom de
     symbole non utilisé, et qu'il n'apparaît en tête d'aucun nom de symbole.

   - L'algorithme présenté ci-dessous prend en argument un ensemble E_DK de
     triplets de termes Dedukti de la forme (LHS, RHS, c), noté LHS  ↪c  RHS.

 Notations :
   - head_<k> : Fonction qui renvoie le symbole de tête de la cellule <k>,
                pour une règle donnée, sans considérer les symboles dotk,
                kseq et inj.
       Il s'agit de la définition de symbole de tête que nous considérons pour
       une règle de réécriture donnée.
   - C_σ : Ensemble des règles qui partagent le même symbole de tête σ,
           soit C_σ = { l ↪c r | head_<k> (l ↪c r) = σ }.
   - X : Nombre de règles conditionnelles dans C_σ.
	 - t_1[t_2]_σ : Substitution de t_2 au sous-terme ayant pour symbole de
                  tête σ dans t_1.
	 - arg_i(t) : i-ème argument de t.
   - arity(t) : Nombre d'arguments de t.
	 - mglhs_σ  : Configuration initiale où les cellules les plus profondes
                deviennent
                  <c> y_i    si <c> ≠ <k>,
	                <k> (kseq (inj σ  z_1 ... z_(arity(σ)) L) sinon,
                avec y_i et z_i des variables fraîches et, L une K computation.
	* update_diff(σ, ♭σ, s_1, i, s_2) = ♭σ  x_1  ...  x_(arity(σ) + X)
    avec x_j =
		            arg_j(σ)   si  1 <= j <= arity(σ)
		            s_1        si  j = arity(σ) + i
		            s_2        sinon
  * update_same(σ, ♭σ, s) = ♭σ  x_1  ...  x_(arity(σ) + X)
    avec x_j =
	 		          arg_j(σ)    si  1 <= j <= arity(σ)
		            s           sinon

 Algorithme :
   Après avoir construit les C_σ à partir de E_DK, nous déroulons l'algorithme
   présenté ci-dessous, pour chaque C_σ :
     [1.] Si X = 0, C_σ est inchangé et l'algorithme s'arrête.
          Sinon, initialiser i à 0 et aller en 2.
     [2.] Générer le LHS le plus général pour un symbole σ donné, noté mglhs_σ.
     [3.] Générer le symbole étendu ♭σ de type
          T_1 -> ... -> T_{n-1} -> ♭Bool -> ... -> ♭Bool -> T_n,
          avec X argument(s) de type ♭Bool, où ♭Bool = Bool ∪ { ♭ },
          et σ de type T_1 -> ... -> T_n.
	   [4.] Générer la règle de substitution :
             mglhs_σ ↪ mglhs_σ [ update_same(σ, ♭σ, ♭)  ]_σ
	   [5.] Pour chaque règle appartenant à C_σ :
           (a) Si la règle est de la forme l ↪c r ∈ C_σ
               et n'a pas l'attribut owise (avec c ≠ ⊤) :
                 - Incrémenter i de 1
				         - Générer une règle d'initialisation :
                     l [ update_diff(σ, ♭σ, ♭, i, _) ]_σ ↪
			               l [ update_diff(σ, ♭σ, c, i, _) ]_σ
				         - Générer une règle de réduction :
				             l [ update_diff(σ, ♭σ, true, i, _) ]_σ ↪ r

           (b) Si la règle est de la forme l ↪⊤ r ∈ C_σ
               et n'a pas l'attribut owise, générer la règle de réduction :
                  l [ update_same(σ, ♭σ, _)  ]_σ ↪ r.

           (c) Si la règle l ↪⊤ r ∈ C_σ à l'attribut owise,
               générer la règle de réduction :
	  		          l [ update_same(σ, ♭σ, false)  ]_σ ↪ r. *)

(**	Extension de l'encodage à l'attribut owise
    ------------------------------------------

    Une manière plus succincte d'écrire l'exemple précédent est
    d'utiliser l'attribut owise :
      rule max X Y => Y requires X <Int Y
      rule max X Y => X [owise]
    Comme K ne génère pas la condition complémentaire dans le fichier Kore,
    nous faisons l'hypothèse que toute fonction construisant un booléen est
    une fonction totale. Sous cette hypothèse, nous pouvons générer la règle
    présentée en 5.(c). *)

open Common.Xlib_OCaml
open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude
open Axiom

(** A supposed safe prefix, i.e. there is no name beginning with it. *)
let safe_prefix = "♭"

(** The term ♭ *)
let p_FLAT = create_ident safe_prefix

(** [p_INJD_appl_ident s] creates the term δ s. *)
let p_INJD_appl_ident : string -> p_term = fun s ->
  create_appl p_INJD (create_ident s)

(** The name ♭Bool *)
let _flatBool = safe_prefix ^ "Bool" (* TODO fix BOOL ?? *)

(** The term δ ♭Bool *)
let p_flatBool = p_INJD_appl_ident _flatBool

(** The name ♭inj *)
let _flatINJ = safe_prefix ^ _INJ

(** The term ♭inj *)
let p_flatINJ = create_ident _flatINJ

(** [p_flatINJ_appl s] creates the term ♭inj s. *)
let p_flatINJ_appl : p_term -> p_term = fun t -> create_appl p_flatINJ t

(** ------------------------------ *)
(** To create each C_σ from a CTRS *)
(** ------------------------------ *)

(** Type of equivalence class related to the head symbol named σ,
    i.e. a map where each entry has the form
    σ |-> [((LHS, RHS), Some condition, priority) ; ...].
    For a head symbol σ given, C_σ is the corresponding equivalence class. *)
type equiv_class = (ctrs_rule list) StrMap.t

(** [is_cell s] returns if a string [s] is a cell's name. *)
let is_cell : string -> bool = fun s ->
  let len = String.length s in
  try
    if !Interface.Output.readable
    then s.[0] = '<' && s.[len-1] = '>'
    else
      String.sub s 0 9 = "Lbl'-LT-'"
      && String.sub s ((String.length s)-6) 6 = "'-GT-'"
  with _ -> false

let is_to_keep : string -> bool = fun s ->
  s = _KSEQ || s = _DOTK || s = _INJ

(** [get_head_symbol _ config] is the function head_<k>, i.e. returns:
        - None si pas de configuration
        - Some _ sinon. *)
let get_head_symbol _ config =
  let rec aux : p_term -> p_term option = fun t ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let res = aux t1 in
        match res with
        | None   -> aux t2
        | Some x -> Some x)
    | P_Patt _ -> None
    | P_Expl _ -> None
    | P_Iden (name, _) as t ->
       let n = snd name.elt in
       if is_to_keep n then
         None
       else
         (if is_cell n then None else Some (no_pos t))
    | _ -> failwith "ERROR"
  in
  aux config

(** [find_equiv_class ec t] adds the p_term [t] into the equivalence
    class [ec].  *)
let find_equiv_class : equiv_class -> ctrs_rule -> equiv_class =
  fun ec (({elt=(lhs,_);_},_,_) as r) ->
  let key = match get_head_symbol (ref 0) lhs with
    | None -> "hum"   (* raise  KCellNotFound *)
    | Some {elt=(P_Iden({elt=(_,x);_},_));_} -> x
    | _ -> failwith "Internal error"
  in
  (*
    update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    update key f m returns a map containing the same bindings as m, except for the binding of key.
    Depending on the value of y where y is f (find_opt key m), the binding of key is added, removed or updated.
    If y is None, the binding is removed if it exists;     otherwise,
    if y is Some z then key is associated to z in the resulting map.
    If key was already bound in m to a value that is physically equal to z,
    m is returned unchanged (the result of the function is then physically equal to m).
   *)
  let f a = match a with
    | None   -> Some [r]   (* Si l'entrée n'existait pas encore *)
    | Some l -> Some(r::l) (* Si l'entrée existait déjà *)
  in
  StrMap.update key f ec

(** [to_equiv_class rule_l] generates each equivalence class from
    a CTRS [rule_l], i.e. each C_σ. *)
let to_equiv_class : ctrs_rule list -> equiv_class = fun rule_l ->
  List.fold_left find_equiv_class StrMap.empty rule_l

(** ----------------------------- *)
(** To iterate on a configuration *)
(** ----------------------------- *)

(* TODO used it?
exception KCellNotFound
exception KCellNotFoundHere *)

(** [has_infered_configuration t] returns true is the term [t]
    is composed of cells, i.e. the configuration has been infered
    durinng the translation from K to Kore. *) (* TODO correct ? *)
let rec has_infered_configuration : p_term -> bool = fun t ->
  match t.elt with
  | P_Appl(t,_)    -> has_infered_configuration t
  | P_Iden(name,_) -> is_cell (snd name.elt)
  | _ -> false

let update_config : string -> p_term -> (p_term -> p_term) -> p_term =
  fun head config f ->
  let rec aux : bool -> p_term -> bool * p_term = fun is_head t ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let l_is_head, x1 = aux is_head t1 in
        let r_is_head, x2 = aux is_head t2 in
        if r_is_head then
          (* (if l_is_head TODO Correcte ?
           then failwith "Several head symbols..."
           else *) false, create_appl x1 (f x2)
        else l_is_head, create_appl x1 x2)
    | P_Patt _ | P_Expl _ -> false, t
    | P_Iden(({elt=(x1,n);pos=y}), x2) ->
       if n = head
       then true,  no_pos (P_Iden(({elt=(x1, safe_prefix ^ n);pos=y}), x2))
       else false, t
    | _ -> failwith "ERROR"
  in
  let res = snd(aux false config) in
  if has_infered_configuration config then res else f res

(** ----------------------------------------------- *)
(** To generate the most general LHS for a symbol σ *)
(** ----------------------------------------------- *)

(** [is_k_cell s] returns if a string [s] is the cell's name
    of the cell k. *)
let is_k_cell : string -> bool = fun s ->
  if !Interface.Output.readable
  then s = "<k>"
  else s = "Lbl'-LT-'k'-GT-'"

(* TODO improve *)
(** [create_most_general_LHS t] transforms the initial configuration [t]
    as follow:
       <c> y_i    if <c> ≠ <k>,
	     <k> (kseq (inj σ z_1 ... z_(arity(σ)) L) otherwise,
    where y_i and z_i are fresh variables and, L is a K computation.
    The result is noted mglhs_σ. *)
let create_most_general_LHS t =
  let nb = ref 0 in
  let new_var nb =
    incr nb ; create_pattern_var ("x" ^ string_of_int !nb)
  in
  let rec aux : p_term -> bool -> bool -> bool * bool * bool * p_term =
    fun t is_in_k_cell is_head ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let l_is_in_k_cell, l_merged, l_is_head, x1 =
          aux t1 is_in_k_cell is_head
        in
        if l_merged then is_in_k_cell, l_merged, l_is_head, new_var nb
        else
          let r_is_in_k_cell, r_merged, r_is_head, x2 =
            aux t2 l_is_in_k_cell l_is_head
          in
          let res =
            if r_merged then create_appl x1 (new_var nb)
            else create_appl x1 x2
          in
          is_in_k_cell, l_merged && r_merged, l_is_head || r_is_head, res)
    | P_Patt _ as t -> is_in_k_cell, true, is_head, no_pos t
    | P_Expl _ as t -> is_in_k_cell, not(is_in_k_cell), is_head, no_pos t
    | P_Iden (name, _) as t ->
       let n = snd name.elt in
       if is_to_keep n then
         is_in_k_cell, not(is_in_k_cell), is_head, no_pos t
       else
         (if is_cell n then
            (is_k_cell n), false, is_head, no_pos t
          else
            if is_head
            then is_in_k_cell, true,  is_head, no_pos t
            else is_in_k_cell, false, true, no_pos t)
    | _ -> failwith "ERROR"
  in
  let _,_,_,res = aux t (not(has_infered_configuration t)) false in res

(** --------------------------------------------------- *)
(** To generate the ♭-symbol of the current head symbol *)
(** --------------------------------------------------- *)

(** [create_list n default_sym i special_sym] creates a list with
    [n] symbols [default_sym], except the [i]e which is [special_sym].
    More precisely:
     - 0 <= n ==> List.length @result = n
     - n <= 0 ==> List.length @result = 0
     - \forall k. k <> i /\ 0 <= k < n ==> @result.[k] = default_sym
     - 0 <= i < n ==> @result.[i] = special_sym
     - Remark: n <= i ==> (\forall k. 0 <= k < n ==> @result.[k] = default_sym) *)
let create_list n default_sym i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym = if current = i then special_sym else default_sym in
      if current = (n-1) then
        [adding_sym]
      else
        adding_sym::(aux (current+1))
    in
    aux 0

(** [create_list_iter nb default_sym] creates a list, thanks to [create_list],
    with [nb] symbols [default_sym]. *)
let create_list_iter nb default_sym =
  (* nb < nb+1, so the list has only [default_sym] element *)
  create_list nb default_sym (nb+1) default_sym

(** [create_carrier_symbol_type nb] generates the type:
    [K] -> [♭Bool] -> ... -> [♭Bool] -> [K], which has [nb+1] arguments.
    Note: ♭Bool = {♭, "true", "false"}. *) (* TODO update *)
let extend_type typ nb =
  let split_type : p_term -> p_term list * p_term = fun typ ->
    let rec aux (t : p_term) acc = match t.elt with
      | P_Type   -> [], t
      | P_Iden _ -> [], t
      | P_Appl _ -> [], t
      | P_Arro(t1, ({elt=P_Type  ;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Iden _;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Appl _;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Arro _;_} as t2)) -> aux t2 (t1::acc)
      | _ -> failwith "Unexpected type which to be extended during Viry's transformation"
    in
    aux typ []
  in
  let arg_type, output_type = split_type typ in
  List.fold_right create_arrow (arg_type@(create_list_iter nb p_flatBool)) output_type

(** --------------------------- *)
(** To generate rewriting rules *)
(** --------------------------- *)

(** [with_one_diff_value heading nb i special_sym] creates left- or
    right-hand-side of the form:
    [heading] _ ... _ [special_sym] _ ... _
    where [heading] has [nb] argument(s),
    and [special_sym] are only at the position [i]. *)
let with_one_diff_value heading nb i special_sym =
  List.fold_left create_appl heading
    (create_list nb p_WILD i special_sym)

(** [with_all_same_value heading nb default_sym] creates left- or right-hand-side
    of the form: [heading] [default_sym] ... [default_sym]
    where [heading] has [nb] argument(s). *)
let with_all_same_value heading nb default_sym =
  List.fold_left create_appl heading (create_list_iter nb default_sym)

(** [create_encapsulation_rule mglhs carrier_sym nb]
    creates an encapsulation rule, i.e. a rule of the form:
    rule [mglhs] ↪ [carrier_sym] [mglhs] ♭ ... ♭
    where [nb] occurrence(s) of ♭. *) (* TODO update *)
let create_encapsulation_rule tracker config nb : p_rule =
  let f h = with_all_same_value h nb p_FLAT in
  create_rule config (tracker config f)

let create_list_number n i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym =
        if current = i then special_sym
        else create_pattern_var ("y" ^ (string_of_int i)) in
      if current = (n-1) then [adding_sym] else adding_sym::(aux (current+1))
    in
    aux 0

    (** [create_initialisation_rule carrier_sym lhs nb i special_sym_l special_sym_r]
    creates an initialisation rule, i.e. a rule of the form:
    rule [carrier_sym] [lhs] _ ... _ [special_sym_l] _ ... _ ↪
         [carrier_sym] [lhs] _ ... _ [special_sym_r] _ ... _
    where [carrier_sym] [lhs] has [nb] argument(s),
    and [special_sym_l] and [special_sym_r] only occur at the position [i]. *) (* TODO update *)
let create_initialisation_rule tracker nb i special_sym_l special_sym_r : p_rule =
  let f special_sym h =
    List.fold_left create_appl h (create_list_number nb i special_sym)
  in
  create_rule (tracker (f special_sym_l)) (tracker (f special_sym_r))

(** [create_reduction_rule carrier_sym lhs nb i special_sym rhs]
    creates a reduction rule, i.e. a rule of the form:
    rule [carrier_sym] [lhs] _ ... _ [special_sym] _ ... _ ↪ [rhs]
    where [carrier_sym] [lhs] has [nb] argument(s),
    and [special_sym] only occurs at the position [i]. *)  (* TODO update *)
let create_reduction_rule tracker nb i special_sym rhs : p_rule =
  let f h = with_one_diff_value h nb i special_sym in
  create_rule (tracker f) rhs

  (** [create_otherwise_rule carrier_sym lhs nb rhs]
    creates an otherwise rule, i.e. a rule of the form:
    rule [carrier_sym] [lhs] "false" ... "false" ↪ [rhs]
    where [nb] occurrence(s) of "false". *) (* TODO update *)
let create_otherwise_rule tracker nb rhs : p_rule =
  let f h =
    with_all_same_value h nb (p_flatINJ_appl p_FALSE)
  in
  create_rule (tracker f) rhs

(** ------------------ *)
(** The main algorithm *)
(** ------------------ *)

(** To store the type of each symbol *)
(* let symb_signature : p_term StrMap.t ref = ref StrMap.empty *)

let viry_encoding : ctrs_rule list -> p_term StrMap.t -> p_symbol list * p_rule list = fun l sign ->
  (* [0.] Create the initial data (♭Bool, ♭, ♭inj, and each C_σ). *)
     (* [a.] Create the symbol ♭Bool. *)
  let flat_bool_sym = create_p_symbol [] _flatBool [] (Some p_SORTK) None in
     (* [b.] Create the symbol ♭. *)
  let flat_sym = create_p_symbol [] safe_prefix [] (Some p_flatBool) None in
     (* [c.] Create the symbol ♭inj. *)
  let flat_inj_type = create_arrow (p_INJD_appl_ident "SortBool") p_flatBool in
  (* δ SortBool → δ ♭Bool *)
  let flat_inj_sym = create_p_symbol [] _flatINJ [] (Some flat_inj_type) None in
     (* [d]. Create each C_σ from a CTRS. *)
  let equiv_class = to_equiv_class l in
  (* For each C_σ *)
  let aux_sigma : string -> ctrs_rule list -> p_symbol list * p_rule list -> p_symbol list * p_rule list =
    fun head_name l (acc_sym, acc_rule) ->
    let tracker = update_config head_name in
    (* [1.] Compute the number of conditions in C_σ, in order to know
            if there is no conditional rule. *)
    let nb_cond = List.fold_left (fun acc (_,data,_) -> match data with | Cond _ -> 1 + acc | _ -> 0 + acc) 0 l in
    if nb_cond = 0
    then acc_sym, List.map (fun (pr,_,_) -> pr) l@acc_rule
    else
      (* [2.] Generate the most general LHS for a given head symbol σ. *)
      let (pr, _, _) = List.hd l in (* FIX if the list is empty *)
      let mglhs = create_most_general_LHS (fst pr.elt) in
      (* [3.] Generate the ♭-symbol of the current head symbol. *)
      let flat_head_name = safe_prefix ^ head_name in
      let flat_head_type =
        try
          extend_type (StrMap.find head_name sign) nb_cond
        with Not_found -> if StrMap.is_empty sign then raise (Common.Error.InternalError "The symbol TRUE")
                          else Common.Error.wrn_1 Common.Error._STDOUT "symbol %i not found" (StrMap.cardinal sign) ; failwith "Plop"
      in
      let flat_head_sym =
        create_p_symbol [] flat_head_name [] (Some flat_head_type) None
      in
      (* [4.] Generate the encapsulation rule. *)
      let encap_r = create_encapsulation_rule tracker mglhs nb_cond in
      (* [5.] For each rule in C_σ *)
      let rec aux_rule : int -> p_rule list -> ctrs_rule list -> p_rule list = (*   fold_lefti *)
        fun i acc ctrs_l ->
        match ctrs_l with
        | [] -> acc
        (* [a.] If the rule is conditional. *)
        | ({elt=(lhs,rhs);_}, Cond c, _)::q ->
           let curr_tracker = tracker lhs in
           let init_r =
             create_initialisation_rule curr_tracker nb_cond i p_FLAT (p_flatINJ_appl c)
           in
           let reduc_r =
             create_reduction_rule curr_tracker nb_cond i
                       (p_flatINJ_appl p_TRUE) rhs
           in
           aux_rule (i+1) (init_r::reduc_r::acc) q
        (* [b.] If the rule is unconditional. *)
        | ({elt=(lhs,rhs);_}, Uncond, _)::q ->
           let reduc_r =
             create_reduction_rule (tracker lhs) nb_cond i p_WILD rhs
           in
           aux_rule i (reduc_r::acc) q
        (* [c.] If the rule has the attribut "owise". *)
        | ({elt=(lhs,rhs);_}, OwiseRule, _)::q ->
           let owise_r = create_otherwise_rule (tracker lhs) nb_cond rhs in
           aux_rule i (owise_r::acc) q
      in
      let new_acc_sym =
        if List.mem flat_head_sym acc_sym then acc_sym
        else flat_head_sym::acc_sym
      in
      new_acc_sym, aux_rule 0 [encap_r] l@acc_rule
  in
  StrMap.fold aux_sigma equiv_class ([flat_inj_sym;flat_sym;flat_bool_sym], [])
