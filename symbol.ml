open Type
open Syntax
open LP_p_term


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
  let find_ac : attribut list -> prop = fun a_l ->
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


  let kjrhf : attribut list -> p_modifier list * bool = fun a_l ->
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

let get_modifier : attribut list -> p_modifier list = fun attr_l ->
  (* On collecte les informations que l'on peut avoir *)
  let rec aux l acc = match l with
    | [] -> acc
    | t::q -> match t with
              | Constructor _ -> aux q (set_constructor acc true)
              | Injective   _ -> aux q (set_injec acc true)
              | Assoc       _ -> aux q (set_assoc acc true)
              | Comm        _ -> aux q (set_comm  acc true)
              | Left        _ -> aux q (set_left  acc true)
              | Right       _ -> aux q (set_right acc true)
              | _             -> aux q acc
  in
  let tmp = aux attr_l no_information in
  (* On traduit ces informations en p_modifier list *)
  let b = get_left tmp in
  let f_prop x = Pos.none (P_prop x) in
  let res = [] in
  let res = if get_injec tmp && not(get_constructor tmp) then f_prop(Injec)::res else res in
  let res = if get_assoc tmp && get_comm tmp then f_prop(AC(b))::res
            else
              if get_assoc tmp then f_prop(Assoc(b))::res
              else
                if get_comm tmp then f_prop(Commu)::res
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

let is_constructor : symbol -> attribut list -> sort option = fun s attri_l ->
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
     Printf.fprintf stdout "A constructor not injective! (%s)\n" (get_name s) ; None




let rec sym_curry : symbol -> p_term = fun s ->
  let _, _, p_l, p = s in
  (**let f = fun (a:p_term) (b:axiom) : p_term ->
                        Pos.none (P_Arro(a, sym_curry b))
        in*)
  let g = fun a -> match a with | S x | Q x ->
                                   create_ident x in
  let f = fun a b -> match a with | S x | Q x ->
                                     Pos.none (P_Arro(create_ident x,b)) in
  List.fold_right f p_l (g p)



let symbol_to_p_symbol : symbol -> attribut list -> p_symbol = fun s attr_l ->
  let name, qvar_l, p_l, p = s in
  (* Merge qvar_l and p_l *)
  let f = fun b a -> [Some (Pos.none a)], Some (Pos.none P_Type), b in
  let qvar_l = List.map (f true) qvar_l in
  let f = fun b a -> match a with | S x | Q x -> f b x in
  let p_l = List.map (f false) p_l in
  (* Transformation of p *)
  let f p = match p with S x | Q x -> x in
  { p_sym_mod = get_modifier attr_l (* ; TODO modifiers *)
  ; p_sym_nam = Pos.none name
  ; p_sym_arg = qvar_l (* qvar_l @ p_l *)
  ; p_sym_typ = Some (sym_curry s)
  (* Some (Pos.none (P_Iden (Pos.none ([""], f p), false)))*)
  ; p_sym_trm = None (* TODO ? *)
  ; p_sym_prf = None
  ; p_sym_def = false }
