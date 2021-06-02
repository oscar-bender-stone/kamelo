
open Type
open Pos
open Arg
open Syntax

open Printer

open Axiom
open Symbol

let input = ref stdin
let c_prefix = ref "a.out"
let c_D = ref false
let c_A = ref false
let c_C = ref false
let c_S = ref false

let basename s =
  try String.sub s 0 (String.rindex s '.')
  with Not_found -> s

let check_extension s =
  let len = String.length s in
  if len > 6 then
    (if not (s.[len-5] = '.' && s.[len-4] = 'k' && s.[len-3] = 'o' && s.[len-2] = 'r' && s.[len-1] = 'e') then
       raise (Invalid_argument "Expected extension: .kore"))
  else
    raise (Invalid_argument "Name file very short")

module Sort = struct
  type t = sort
  let compare = String.compare
end
module Induc = Map.Make(Sort)

;; (* ( sort |-> symbol list) *)

let () =
  parse
    [(*("-v",  Unit (fun () -> verbose:=1), "reports stuff");
     ("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("-D",  Unit (fun () -> c_D:=true),  "print declarations");
     ("-A",  Unit (fun () -> c_A:=true),  "print abstract syntax tree");
     ("-C",  Unit (fun () -> c_C:=true),  "print abstract syntax tree close to the C code");
     ("-S",  Unit (fun () -> c_S:=true),  "output assembler dump")]
    (fun s ->
      check_extension s;
      c_prefix := basename s;
      input := open_in s) "compiles a .kore program"

let () =
  let lexbuf = Lexing.from_channel (!input) in
  let f = Kparser.file Klexer.token lexbuf in
  (*let rec print_c c = match c with
    | [] -> Format.fprintf Format.std_formatter "\n"
    | a::t -> Format.fprintf Format.std_formatter "%a" pp_command a; print_c t
  in
  print_c c;*)

  let import_to_require_open : string list -> import -> p_command = fun chemin i ->
    let filename = String.lowercase_ascii (fst i) in
    Pos.none (P_require (true, [Pos.none (chemin @ [filename])]))
  in
  let sort_to_p_symbol : sort -> p_symbol = fun s ->
    { p_sym_mod = [] (* Const ? *)
    ; p_sym_nam = Pos.none s
    ; p_sym_arg = []   (* TODO *)
    ; p_sym_typ = Some (Pos.none P_Type) (* TODO, after ? TYPE ? K ? *)
    ; p_sym_trm = None
    ; p_sym_prf = None
    ; p_sym_def = false }
  in

  let induc_to_p_inductive : sort * symbol list -> p_inductive = fun (sort, s_l) ->
    (* p_ident * p_term * (p_ident * p_term) list *)
   let f s = (Pos.none (get_name s), sym_curry s) in
   Pos.none (Pos.none sort, Pos.none P_Type, List.map f s_l)

  (* let param_to_p_params : param -> p_params = fun p -> *)
  in

  let trans_import : Format.formatter -> import -> unit = fun ppf i ->
    pp_command ppf (import_to_require_open ["tests"] i)
  in
  let trans_sort : Format.formatter -> sort -> unit =
    fun ppf s -> pp_command ppf (Pos.none (P_symbol (sort_to_p_symbol s)))
  in
  let trans_induc : Format.formatter -> sort * symbol list -> unit =
    fun ppf i -> pp_command ppf (Pos.none (P_inductive([], [], [induc_to_p_inductive i])))
  in
  let trans_symbol : Format.formatter -> symbol * attribut list -> unit =
    fun ppf (s, attr_l) -> pp_command ppf (Pos.none (P_symbol (symbol_to_p_symbol s attr_l)))
  in
  let trans_axiom : Format.formatter -> quant_var list * axiom * attribut list -> unit =
    fun ppf (qv_l, a, attr_l) ->
    match attr_l with
     | Unit _::nil | Comm _::nil | Assoc _::nil | Idem _::nil ->
        pp_command ppf (Pos.none (P_rules [of_equality_axiom a]))
     | _ -> () (* @TODO *)
  in
  (*let trans_axiom : Format.formatter -> axiom -> unit =*)

  let trans_command : Format.formatter -> command -> unit =
    fun ppf (c, attri_l) ->
    match c with
     | Sort   s | H_sort   s ->
        pp_command ppf (Pos.none (P_symbol (sort_to_p_symbol s)))
     | Symbol s | H_symbol s ->
        pp_command ppf (Pos.none (P_symbol (symbol_to_p_symbol s attri_l)))
     | Alias  _       -> ()
     | Axiom  (qv, a) ->
        match attri_l with
         | Unit _::nil | Comm _::nil | Assoc _::nil | Idem _::nil ->
            pp_command ppf (Pos.none (P_rules [of_equality_axiom a]))
         | _ -> ()
  in



  (* 1 : remonter les symboles des types inductifs + descendre les sorts des types inductifs
   * 2 : Enlever les axiomes qui ne nous intéressent pas
   * [3] :  Regrouper ce qui va ensemble ? Risque de casser les dépendences ?
   * [4] : Regrouper au moins ce qui relève des configurations ? du sous-typage ?
   **)
  let rec remove : 'a -> 'a list -> 'a list = fun a l ->
    match l with
     | [] -> []
     | t::q -> if t = a then q else t::(remove a q)
  in
  let preprocessing :
        modu -> name * import list * sort list * (symbol list) Induc.t * (symbol * attribut list) list *
                  (quant_var list * axiom * attribut list) list =
    fun (name, i_l, c_l, _) ->
    let rec aux l ((sort_l, induc_m, sym_l, ax_l) as acc) =
      match l with
       | [] -> acc
       | (c, attr_l)::q ->
          match c with
           | Sort   s | H_sort   s -> aux q (s::sort_l, induc_m, sym_l, ax_l)
           | Symbol s | H_symbol s ->
              (match is_constructor s attr_l with
                | Some sort ->
                   let f new_v old_v = match old_v with
                       None -> Some [new_v] | Some q -> Some (new_v::q) in
                   let induc_m = Induc.update sort (f s) induc_m in
                   aux q (remove sort sort_l, induc_m, sym_l, ax_l)
                | None ->
                   aux q (sort_l, induc_m, (s,attr_l)::sym_l, ax_l))
           | Alias _ -> aux q (sort_l, induc_m, sym_l, ax_l) (* @TODO *)
           | Axiom(qv,a) ->
              match attr_l with
               | [] -> if is_predicate_axiom a
                       then aux q acc
                       else aux q (sort_l, induc_m, sym_l, (qv,a,attr_l)::ax_l)
               | [t] ->
                  let res = of_axiom (qv,a,attr_l) t ax_l in
                  aux q (sort_l, induc_m, sym_l, res)
               | _ -> aux q (sort_l, induc_m, sym_l, (qv,a,attr_l)::ax_l)  (* failwith "Not yet implemented" *)
    in
    let sort_l, induc_m, sym_l, ax_l = aux c_l ([], Induc.empty, [], []) in
    (name, List.rev i_l, List.rev sort_l, induc_m, List.rev sym_l, List.rev ax_l)
  in
  let module_to_file : modu -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    let name, import_l, sort_l, induc_m, sym_l, ax_l = preprocessing m in

    let import_l = if Induc.is_empty induc_m then import_l else ("prelude", [])::import_l in

    let filename = String.lowercase_ascii name in
    let f  = open_out (filename ^ ".lp") in
    let ff = Format.formatter_of_out_channel f in
    List.iter (trans_import  ff) import_l;
    List.iter (trans_sort ff) sort_l;
    List.iter (trans_induc ff) (List.rev (Induc.bindings induc_m));
    List.iter (trans_symbol ff) sym_l;
    (*List.iter (trans_command ff) command_l;*)
    Format.printf "There are %i axiom(s) in %s.lp\n" (List.length ax_l) filename;
    List.iter (trans_axiom ff) ax_l;
    (*List.iter (trans_command Format.std_formatter) command_l;*)
    Format.pp_print_flush ff ();
    close_out f
  in
  List.iter module_to_file (snd f);
  flush stdout;;
