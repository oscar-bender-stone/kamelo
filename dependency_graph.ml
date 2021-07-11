
open Common.Type
open Iterator

open Graph
(*open Pack.Digraph*)
(*
module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
(**module G = Persistent.Digraph.ConcreteLabeled(Int)(Int)

let g = G.empty
let g = G.add_vertex g 1
let g = G.add_edge_e g (G.E.create 1 10 2)
let g = G.add_edge_e g (G.E.create 2 50 3)
let g = G.add_edge_e g (G.E.create 1 30 4)
let g = G.add_edge_e g (G.E.create 1 100 5)
let g = G.add_edge_e g (G.E.create 3 10 5)
let g = G.add_edge_e g (G.E.create 4 20 3)
let g = G.add_edge_e g (G.E.create 4 60 5)

let g = G.remove_vertex g 4 *)
(*
let gc = G.add_edge_e g (G.E.create 5 10 1)
let gc = G.add_vertex gc 6
 *)

module G = Persistent.Digraph.Concrete(Int)
(*
module H : Topological with type t = G.t;;
 *)

module H = Graph.Topological.Make(G)

let g = G.empty
let g = G.add_vertex g 1
let g = G.add_edge_e g (G.E.create 1 () 2)
 *)

module Name = struct
  type t = string
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = ""
end

module Gname = Persistent.Digraph.Concrete(Name)
module T = Graph.Topological.Make(Gname)

module Link = Map.Make(String)
let link : kommand Link.t ref = ref Link.empty

let add_node = Gname.add_vertex
let add_egde g n1 n2 =
  try
    Gname.add_edge g n1 n2 (* @TODO cette fonction ne renvoit pas d'erreur.... *)
  with
    Not_found -> Printf.printf "Coucou" ; g

let add_sort g s = link := Link.add s (Sort s,[]) !link ; add_node g s (* @TODO fix *)

let add_dependence g node dep = match dep with
    S s -> add_egde g s node
  | Q _ -> g (* @TODO ?? *)

let update_graph : Gname.t -> 'a list -> (Gname.t -> 'b -> 'a -> Gname.t) -> 'b -> Gname.t = fun g l f node ->
  let rec update_graph_aux g l = match l with
    | [] -> g
    | t::q -> update_graph_aux (f g node t) q
  in
  update_graph_aux g l

let add_symbol g s =
  let n, _, p_l, p = s in
  link := Link.add n (Symbol s, []) !link ; (* @TODO fix *)
  let g = add_node g n in
  (* qv_l @TODO ?? *)
  let g = update_graph g p_l add_dependence n in
  add_dependence g n p

let nb = ref 0

(* qv_l @TODO ??*)
let add_axiom g qv_l ax attr_l =
  incr nb ;
  let ax_node = "ax" ^ string_of_int !nb in
  let rec add_axiom_aux g ax = match ax with
    | Predicat p ->
       (match p with
        | Sym(_, p_l, ax_l)   ->
           let g = update_graph g p_l add_dependence ax_node in
           List.fold_left add_axiom_aux g ax_l
        (* update_graph g ax_l add_axiom_aux ax_node *)
        | Var(_, p) -> add_dependence g ax_node p)
    | Exists(p_l,(_,p),ax) | In(p_l,(_,p),ax) ->
       let g = update_graph g p_l add_dependence ax_node in
       let g = add_dependence g ax_node p in
       add_axiom_aux g ax
    | Equals(p_l,ax1,ax2) | And(p_l,ax1,ax2) | Or(p_l,ax1,ax2) |
      Implies(p_l,ax1,ax2) | Rewrites(p_l,ax1,ax2) ->
       let g = update_graph g p_l add_dependence ax_node in
       let g = add_axiom_aux g ax1 in add_axiom_aux g ax2
    | Not(p_l,ax) ->
       let g = update_graph g p_l add_dependence ax_node in
       add_axiom_aux g ax
    | Bottom p_l | Top p_l ->
       update_graph g p_l add_dependence ax_node
    | Dom_val _ -> g
  in
  link := Link.add ax_node (Axiom (qv_l,ax), attr_l) !link ; (* @TODO FIX*)
  add_axiom_aux (add_node g ax_node) ax

let deleted : kommand Link.t ref = ref Link.empty

let create_dependence_graph cd l =
  let init_graph = Gname.empty in
  let f_hooked_symbol attr_l g s =
    deleted := Link.add (Symbol.get_name s) (H_symbol s, attr_l) !deleted ;
    g
  in
  let f_rewrite attr_l g ({lhs=_;rhs=(qv_l,ax)}) =
      add_axiom g qv_l ax attr_l (* @TODO forget alias *)
  in
  let do_nothing = fun _ g _ -> g in
  kommand_iter_without_alias cd l init_graph
    (fun _ g s -> add_sort   g s) do_nothing
    (fun _ g s -> add_symbol g s) f_hooked_symbol
    do_nothing f_rewrite
    (fun attr_l g (qv_l, ax) -> add_axiom g qv_l ax attr_l)
    (do_nothing, do_nothing, do_nothing, do_nothing, do_nothing, do_nothing,
     do_nothing, do_nothing, do_nothing, do_nothing, do_nothing)

      (*
let () =

  H.iter (fun _ -> Format.printf "Hello") g (* ([1,2;4,5;5,1])**)
 *)
