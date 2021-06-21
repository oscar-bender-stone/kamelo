
open Type
open Pos
open Arg
open Syntax

open LP_printer
open Printer

open Axiom
open Symbol
open LP_p_term

open Display_console
open Output

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

let check_induc = ref false

let lp_pkg = "tests"
let prelude_path = ["Tests"] (* depuis lp_pkg *)
let prelude_name = "prelude"

let get_filename name =
  let tmp = String.lowercase_ascii name in
  if !lp_output then tmp ^ ".lp" else tmp ^ ".dk"


type output_management = K | Kore | Dedukti

let set_format o = if o = "K" || o = "k" then k_format := true
                   else
                     if o = "Kore" || o = "kore" then kore_format := true
                     else
                       if o = "Dedukti" || o = "dedukti" then dk_format := true
                       else failwith ("The option" ^ o ^ "is unknow")

let set_output o = if o = "Dedukti" || o = "dedukti" then lp_output := false
                   else
                     if o = "Lambdapi" || o = "lambdapi" then lp_output := true
                     else failwith ("The option"^ o ^ "is unknow")
let () =
  let usage_msg = "usage: ./kamelo [-f (K|Kore|Dedukti)] [-o (Dedukti|Lambdapi)] [--inductive] [--readable] [--no-color] kore_file" in
  parse
    [("--format",  String (fun o -> set_format o),  "Change the ordering of commands");
     ("-f",  String (fun o -> set_format o),  "Change the ordering of commands");
     ("--output",  String (fun o -> set_output o), "Change the output: .dk file or .lp file");
     ("-o", String (fun o -> set_output o), "Change the output: .dk file or .lp file");

     ("--inductive",  Unit (fun () -> check_induc:=true),  "Use inductive types");
     ("-i",           Unit (fun () -> check_induc:=true),  "Use inductive types");

     ("--readable",  Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     ("-r",  Unit (fun () -> readable:=true),  "Generate identifiers more readable");
     (*("-v",  Unit (fun () -> verbose:=1), "reports stuff");
     ("-v1", Unit (fun () -> verbose:=1), "reports stuff");
     ("-v2", Unit (fun () -> verbose:=2), "reports stuff, and stuff");*)
     ("--no-color",  Unit (fun () -> no_color:=true),  "Disable colors on the main message")]
    (fun s ->
      check_extension s;
      c_prefix := basename s;
      input := open_in s)
    ("During compilation of a .kore program:\n" ^ usage_msg)  (* Format.printf "%s" usage_msg "During compilation of a .kore program"*)

let () =
  let lexbuf = Lexing.from_channel (!input) in
  let file = Kparser.file Klexer.token lexbuf in
  (*let rec print_c c = match c with
    | [] -> Format.fprintf Format.std_formatter "\n"
    | a::t -> Format.fprintf Format.std_formatter "%a" pp_command a; print_c t
  in
  print_c c;*)

  (* let param_to_p_params : param -> p_params = fun p -> *)





  (*let trans_axiom : Format.formatter -> axiom -> unit =*)

  (* let trans_command : Format.formatter -> command -> unit =
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
  in *)



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
  let print_new_attribute : name -> attribute list -> unit = fun name attri_l ->
    let rec aux = fun l acc ->
      match l with
       | [] -> acc
       | t::q -> (match t with
                   | Other(n,_) -> aux q (n::acc)
                   | _ -> aux q acc)
    in
    let res = aux attri_l [] in
    match res with
     | [] -> ()
     | t::q as l ->
        Format.printf (yel "WARNING: The symbol %s has new attribut(s): ") name;
        List.iter (fun n -> Format.printf (yel "%s ") n) l;
        Format.printf (yel ".\n")
  in
  let preprocessing :
        kmodule -> count_data ->
        name * import list * sort list * (symbol list) Induc.t * (symbol * attribute list) list *
        (alias * (quant_var list * axiom * attribute list) option) list *
        (quant_var list * axiom * attribute list) list =
    fun (name, i_l, c_l, _) cd ->
    let rec aux l ((sort_l, induc_m, sym_l, alias_l, ax_l) as acc) =
      match l with
       | [] -> acc
       | (c, attr_l)::q ->
          match c with
           | Sort   s ->
              incr_k_sort cd ; print_new_attribute s attr_l ;
              aux q (s::sort_l, induc_m, sym_l, alias_l, ax_l)
           | H_sort s -> incr_k_hooked_sort cd ; aux q acc
           | Symbol s ->
              begin
                let name,_,_,_ = s in print_new_attribute name attr_l ;
                                      if not(!check_induc) then (incr_k_symbol cd ; aux q (sort_l, induc_m, (s,attr_l)::sym_l, alias_l, ax_l))
                                      else
                                        (match is_constructor s attr_l with
                                          | Some sort ->
                                             let f new_v old_v = match old_v with
                                                 None -> Some [new_v] | Some q -> Some (new_v::q) in
                                             let induc_m = Induc.update sort (f s) induc_m in
                                             aux q (remove sort sort_l, induc_m, sym_l, alias_l, ax_l)
                                          | None ->
                                             aux q (sort_l, induc_m, (s,attr_l)::sym_l, alias_l, ax_l))
              end
           | H_symbol s -> incr_k_hooked_symbol cd ; aux q acc
           | Alias al->
              (match q with
                | [] -> incr_k_alias cd ; aux q (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)
                | h::tl ->
                   (match h with
                     | Axiom(qv,a), attr_l ->
                        if is_rule_axiom a
                        then
                          (incr_k_rule cd ;
                           aux tl (sort_l, induc_m, sym_l, (al, Some(qv,a,attr_l))::alias_l, ax_l))
                        else
                          (incr_k_alias cd ;
                           aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l))
                     | _ -> incr_k_alias cd ; aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)))
           | Axiom(qv,a) ->
              incr_k_axiom cd ;
              match attr_l with
               | [] -> if is_predicate_axiom a
                       then aux q acc
                       else aux q (sort_l, induc_m, sym_l, alias_l, (qv,a,attr_l)::ax_l)
               | [t] ->
                  let res = of_axiom (qv,a,attr_l) t ax_l in
                  aux q (sort_l, induc_m, sym_l, alias_l, res)
               | _ -> aux q (sort_l, induc_m, sym_l, alias_l, (qv,a,attr_l)::ax_l)  (* failwith "Not yet implemented" *)
    in
    let sort_l, induc_m, sym_l, alias_l, ax_l = aux c_l ([], Induc.empty, [], [], []) in
    (name, List.rev i_l, List.rev sort_l, induc_m, List.rev sym_l, List.rev alias_l, List.rev ax_l)
  in

  let module_to_file : kmodule -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    let len = List.length in

    let cd = reset_count_data 0 in

    let name, kimport_l, kcommand_l, _ = m in
    cd.k_import := len (kimport_l) ;
    let filename = get_filename name in
    print_header_file filename;

    print_nb_total_commands (len kcommand_l);

    let _, import_l, sort_l, induc_m, sym_l, alias_l, ax_l = preprocessing m cd in

    (* let import_l = if Induc.is_empty induc_m then import_l else ("prelude", [])::import_l in *)

    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in

    List.iter (pp_import ff cd [lp_pkg]) import_l;
    pp_import ff  cd (lp_pkg::prelude_path) (prelude_name, []);
    List.iter (pp_sort ff cd) sort_l;
    List.iter (pp_induc ff cd) (List.rev (Induc.bindings induc_m));
    List.iter (pp_symbol ff cd) sym_l;
    (*List.iter (trans_command ff) command_l;*)
    List.iter (pp_alias ff cd) alias_l;
    List.iter (pp_axiom ff cd) ax_l;
    (*List.iter (trans_command Format.std_formatter) command_l;*)

    print_count_data cd;

    print_separator ();
    Format.pp_print_flush ff ();
    close_out f
  in
  print_header_kamelo ();
  List.iter module_to_file (snd file);
  print_footer_kamelo ();
  flush stdout;;
