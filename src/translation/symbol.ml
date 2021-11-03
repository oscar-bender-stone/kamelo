open Common.Type
open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude

open Common.Color

let has_no_param : symbol -> bool = fun (_, _, p_l, _) ->
  match p_l with
  | [] -> true
  | _  -> false


(*
  (* Etre constant n'implique pas qu'on n'est pas une fonction : cela
   * implique juste qu'on ne peut plus réduire le symbole.
   * Un symbole qui n'a pas de paramètre est donc constant (on veut le
   * traduire comme cela ?), mais pas l'inverse.
   * En K, on est constant si notre type est un sous-type du type KResult ? *)
  let is_constant

   (is_constant, is_injective, is_constructor, is_comm, is_assoc,
    is_left, is_right) *)
type t = { is_constant    : bool
         ; is_injective   : bool
         ; is_constructor : bool
         ; is_comm        : bool
         ; is_assoc       : bool
         ; is_left        : bool
         ; is_right       : bool }

let no_information = { is_constant    = false
                     ; is_injective   = false
                     ; is_constructor = false
                     ; is_comm        = false
                     ; is_assoc       = false
                     ; is_left        = false
                     ; is_right       = false }

let set_injec       : t -> bool -> t = fun v b -> { v with is_injective = b   }
let set_constructor : t -> bool -> t = fun v b -> { v with is_constructor = b }
let set_comm        : t -> bool -> t = fun v b -> { v with is_comm = b        }
let set_assoc       : t -> bool -> t = fun v b -> { v with is_assoc = b       }
let set_left        : t -> bool -> t = fun v b -> { v with is_left = b        }
let set_right       : t -> bool -> t = fun v b -> { v with is_right = b       }

let get_injec       : t -> bool = fun v -> v.is_injective
let get_constructor : t -> bool = fun v -> v.is_constructor
let get_comm        : t -> bool = fun v -> v.is_comm
let get_assoc       : t -> bool = fun v -> v.is_assoc
let get_left        : t -> bool = fun v -> v.is_left
let get_right       : t -> bool = fun v -> v.is_right
           (*
  let find_ac : attribute list -> prop = fun a_l ->
    let rec aux l acc = match l with
     | []  -> acc
     | t::q -> match t with
                | Constructor  -> aux q (set_constructor acc)
                | Injective    -> aux q (set_injec acc)
                | Assoc        -> aux q (set_assoc acc)
                | Comm         -> aux q (set_comm  acc)
                | Left         -> aux q (set_left  acc)
                | Right        -> aux q (set_right acc)
    in
    aux a_l Defin
  in


  let kjrhf : attribute list -> p_modifier list * bool = fun a_l ->
    match t with
     | []  -> []
     | t::q -> match t with
                 | Constructor  -> aux q (set_constructor acc)
                | Injective    -> P_prop(Injec) :: aux q (set_injec acc)
                | Assoc        -> aux q (set_assoc acc)
                | Comm         -> P_prop(Commu) :: aux q (set_comm  acc)
                | Left         -> aux q (set_left  acc)
                | Right        -> aux q (set_right acc)


 *)

let get_modifier : attribute list -> p_modifier list = fun attr_l ->
  (* On collecte les informations que l'on peut avoir *)
  let rec aux l acc = match l with
    | [] -> acc
    | t::q -> match t with
              | Constructor _ -> aux q (set_constructor acc true)
              | Injective   _ -> aux q (set_injec acc true)
              | Assoc       _ -> aux q (set_assoc acc true)
              | Comm        _ -> aux q (set_comm  acc true)
              (*| Left        _ -> aux q (set_left  acc true)
              | Right       _ -> aux q (set_right acc true)*)
              | _             -> aux q acc
  in
  let tmp = aux attr_l no_information in
  (* On traduit ces informations en p_modifier list *)
  let b = get_left tmp in
  let res = [] in
  let res = if get_injec tmp && not(get_constructor tmp) then create_prop(Injec)::res else res in
  let res = if get_assoc tmp && get_comm tmp then create_prop(AC(b))::res
            else
              if get_assoc tmp then res (* you need to generate the rule *)
              else
                if get_comm tmp then create_prop(Commu)::res
                else res
  in
  (* @TODO Const ? *)
  res

let get_param : symbol -> param = fun s ->
  let (_, _, _, p) = s in p

let get_sort : symbol -> sort = fun s ->
  let p = get_param s in
  match p with
  | S s -> s
  | Q _ -> failwith "No sort"

let get_name : symbol -> name = fun s ->
  let (n, _, _, _) = s in n

let is_constructor : symbol -> attribute list -> sort option = fun s attri_l ->
  let rec aux l acc = match l with
    | []   -> acc
    | t::q -> match t with
              | Constructor _ -> aux q (true, snd acc)
              | Injective   _ -> aux q (fst acc, true)
              | _             -> aux q acc
  in
  let is_cons, is_inj = aux attri_l (false, false) in
  match is_cons, is_inj with
  | (false, _) -> None
  | (true, true)   -> Some (get_sort s)
  | (true, false)  ->
     Printf.fprintf stdout (yel "WARNING The symbol (%s) is declared 'constructor' but not 'injective'!\n") (get_name s) ; None

let get_type : string -> p_term = fun s ->
  let p_s = create_ident s in
  if s = _SORTK then p_s else create_appl p_INJD p_s

let sym_curry : symbol -> p_term = fun s ->
  let _, _, p_l, p = s in
  (* let f = fun (a:p_term) (b:axiom) : p_term -> create_arrow a (sym_curry b) in *)
  let g = fun a ->
    match a with
    | S x | Q x -> get_type x
  in
  let f = fun a b ->
    match a with | S x | Q x -> create_arrow (get_type x) b in
  List.fold_right f p_l (g p)

let def_to_p_term : def -> p_term = fun d ->
  match d with
  | A ax    ->
     begin
       match ax with
       | And(_,a1,a2) ->
          if Axiom.is_conditional_rule a1 then
            (* raise (Axiom.ConditionalRule "Conditional rewriting rule not supported yet.")*)
            p_TYPE
          else
            (try Axiom.curry_ident a2
             with Axiom.KComputation _ ->
               Format.printf (yel "WARNING: K computation found\n") ; p_TYPE)
       | Predicate p -> (match p with Sym _ -> p_TYPE | Var _ -> p_TYPE)
       |  _ -> failwith "In LHS: Not yet implemented"
     end
  | D (n,_) -> create_ident n

let param_to_p_term p = match p with S s -> get_type s | Q _ -> p_TYPE

(** [create_p_params_expl l] creates explicit parameters, which have the current given type,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params_expl : (name * param) list -> p_params list = fun s_l ->
  let is_implicit = false in
  let f (n,p) = ([Some (create_p_ident n)], Some (param_to_p_term p), is_implicit)  in
  List.map f s_l

(** [create_p_params s_l] creates implicit parameters, which have the type _SORTK,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params : string list -> p_params list = fun s_l ->
  match s_l with
  | []   -> []
  | _::_ ->
     let unique_name s = Some (no_pos s)  in
     let typ = Some p_SORTK in
     let is_implicit = true in
     [ List.map unique_name s_l, typ, is_implicit ]

let alias_to_definition : alias -> p_command = fun al ->
  let (name, qv_l, _, p), (name_bis, qv_l_bis, expl_l, def) = al in
  let _ =
    if not(name = name_bis) (* && qv_l = qv_l_bis) *) then qv_l else qv_l_bis
  in
  (* STEP 0: Get the signature *)

  (* STEP 1: Get the definition of the symbol *)
  let body = def_to_p_term def in
  (* STEP 2: Build the p_symbol *)
  let p_l = create_p_params qv_l @ (create_p_params_expl expl_l) in
  no_pos (P_symbol (create_p_symbol [] name p_l (Some (param_to_p_term p)) (Some body)))
