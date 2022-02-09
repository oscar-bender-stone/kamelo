
open Error
open Type

(** Getter for command *)

(** For symbol *)

let get_name : symbol -> name = fun s ->
  let (n, _, _, _) = s in n

let has_no_param : symbol -> bool = fun s ->
  let (_, _, p_l, _) = s in
  match p_l with
  | [] -> true
  | _  -> false

let get_param : symbol -> param = fun s ->
  let (_, _, _, p) = s in p

let get_sort : symbol -> sort = fun s -> (* Fix TODO *)
  let p = get_param s in
  match p with
  | S s -> s
  | Q _ -> failwith "No sort"

(** [is_constructor s attr_l] returns:
      - None, if the attribut "constructor" is not in [attr_l]
      - the type of [s] if the attributs "constructor" and
                           "injecitve" are in [attr_l]
      - A warning if the attribut "constructor" is in [attr_l]
                     but not the attribut "injective" *)
let is_constructor : symbol -> attribute list -> sort option =
  fun s attri_l ->
  let rec aux l acc = match l with
   | []   -> acc
   | t::q -> match t with
            | Constructor _ -> aux q (true, snd acc)
            | Injective   _ -> aux q (fst acc, true)
            | _             -> aux q acc
  in
  let is_cons, is_inj = aux attri_l (false, false) in
  match is_cons, is_inj with
  | (false, _)     -> None
  | (true, true)   -> Some (get_sort s)
  | (true, false)  ->
     wrn_1 _STDOUT "WARNING: The symbol (%s) is declared \
                    'constructor' but not 'injective'!" (get_name s) ;
     None



(** For axiom *)

let rec is_predicate : axiom -> bool = fun a ->
  match a with
  | Equals(_,a1,a2)  -> is_predicate a1 || is_predicate a2
  | Exists(_,_,a)    -> is_predicate a
  | And(_,a1,a2)     -> is_predicate a1 || is_predicate a2
  | Or(_,a1,a2)      -> is_predicate a1 || is_predicate a2
  | Not(_,a)         -> is_predicate a
  | Implies(_,a1,a2) -> is_predicate a1 || is_predicate a2
  | Bottom   _  -> false
  | Top      _  -> false
  | Rewrites _  -> false (* users' rule *)
  | In(_,_,a)        -> is_predicate a
  | Dom_val  _  -> false
  | Predicate p -> match p with
                   | Sym(n, _, _) -> (* @TODO (n,_,a_l) ? *)
                      begin
                       try
                         let res = String.sub n 0 5 in String.equal res "Lblis"
                       with _ -> false
                      end
                   | Var _ -> false

let is_rule : axiom -> bool = fun a ->
  match a with
  | Rewrites _ -> true
  | _ -> false

let is_conditional_rule : axiom -> bool = fun a ->
  match a with
  | Top _ -> false
  | _     -> true

let is_cooling_rule : attribute list -> bool = fun l ->
  let f a = match a with
    | Cool _ -> true
    | _ -> false
  in
  List.fold_left (fun acc x -> f x || acc) false l

let is_heating_rule : attribute list -> bool = fun l ->
  let f a = match a with
    | Heat _ -> true
    | _ -> false
  in
  List.fold_left (fun acc x -> f x || acc) false l
