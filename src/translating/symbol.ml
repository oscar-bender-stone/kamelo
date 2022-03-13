open Common.Type
open LP.Syntax
open Interface.LP_p_term
open Interface.Getter_term

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

let get_modifier : attribute list -> p_modifier list = fun attr_l ->
  (* Collect of properties *)
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
  (* Translation into p_modifier list *)
  let b = get_left tmp in
  let res =
    if get_injec tmp && not(get_constructor tmp)
    then [create_prop(Injec)]
    else []
  in
  let res = if get_assoc tmp && get_comm tmp then create_prop(AC(b))::res
            else
              if get_assoc tmp then res (* you need to generate the rule *)
              else
                if get_comm tmp then create_prop(Commu)::res
                else res
  in (* @TODO Const ? *)
  res

(** To translate symbol *)

let curry_symbol : symbol -> p_term = fun s ->
  let _, _, p_l, p = s in
  (* let f = fun (a:p_term) (b:axiom) : p_term -> create_arrow a (curry_symbol b) in *)
  let g = fun a ->
    match a with
    | S x | Q x -> create_type_atomic x
  in
  let f = fun a b ->
    match a with | S x | Q x -> create_arrow (create_type_atomic x) b in
  List.fold_right f p_l (g p)

let symbol_to_p_symbol : symbol -> attribute list -> p_command =
  fun s attr_l ->
  let name, qvar_l, _, _ = s in
  let param_l = create_p_params qvar_l in
  let res =
    create_p_symbol (get_modifier attr_l) name param_l
      (Some (curry_symbol s)) None
  in
  create_LP_symbol res
