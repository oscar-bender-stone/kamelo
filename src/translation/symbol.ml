open Common.Type
open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude


(*
  (* Etre constant n'implique pas qu'on n'est pas une fonction : cela
   * implique juste qu'on ne peut plus réduire le symbole.
   * Un symbole qui n'a pas de paramètre est donc constant (on veut le
   * traduire comme cela ?), mais pas l'inverse.
   * En K, on est constant si notre type est un sous-type du type KResult ? *)
  let is_constant

   (is_constant, is_injective, is_constructor, is_comm, is_assoc,
    is_left, is_right) *)

(** To translate a list of attributes into a property *)

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

let set_injec       : t -> bool -> t = fun v b ->
  { v with is_injective = b   }
let set_constructor : t -> bool -> t = fun v b ->
  { v with is_constructor = b }
let set_comm        : t -> bool -> t = fun v b ->
  { v with is_comm = b        }
let set_assoc       : t -> bool -> t = fun v b ->
  { v with is_assoc = b       }
(*let set_left        : t -> bool -> t = fun v b ->
   { v with is_left = b        }
let set_right       : t -> bool -> t = fun v b ->
  { v with is_right = b       }*)

let get_injec       : t -> bool = fun v -> v.is_injective
let get_constructor : t -> bool = fun v -> v.is_constructor
let get_comm        : t -> bool = fun v -> v.is_comm
let get_assoc       : t -> bool = fun v -> v.is_assoc
let get_left        : t -> bool = fun v -> v.is_left
(*let get_right       : t -> bool = fun v -> v.is_right *)
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

(** To translate symbol *)

(** [cr_type s] creates the type :
      - _SORTK       if s = _SORTK
      - p_INJD (f s) otherwise
    Note: f transforms s into a p_term. *)
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
