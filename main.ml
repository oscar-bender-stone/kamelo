
open Type
open Pos
open Arg
open Syntax

open Printer

open Axiom
open Symbol
open LP_p_term

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

let nb_import = ref 0
let nb_sort   = ref 0
let nb_symbol = ref 0
let nb_alias  = ref 0
let nb_rule   = ref 0
let nb_axiom  = ref 0

let real_import = ref 0
let real_sort   = ref 0
let real_induc  = ref 0
let real_symbol = ref 0
let real_induc  = ref 0
let real_alias  = ref 0
let real_rule   = ref 0
let real_axiom  = ref 0

let import_to_require_open : string list -> import -> p_command = fun chemin i ->
  let filename = String.lowercase_ascii (fst i) in
  Pos.none (P_require (true, [Pos.none (chemin @ [filename])]))

let pp_import : Format.formatter -> string list -> import -> unit =
  fun ppf path i -> incr real_import ; pp_command ppf (import_to_require_open path i)

let get_sort_type : sort -> p_term = fun s ->
  if s = "SortK" then Pos.none P_Type else create_ident "SortK"

let sort_to_p_symbol : sort -> p_symbol = fun s ->
  let sort_type = get_sort_type s in
  { p_sym_mod = [] (* Const ? *)
  ; p_sym_nam = Pos.none s
  ; p_sym_arg = []   (* TODO *)
  ; p_sym_typ = Some sort_type (* TODO, after ? TYPE ? K ? *)
  ; p_sym_trm = None
  ; p_sym_prf = None
  ; p_sym_def = false }

let pp_sort : Format.formatter -> sort -> unit =
  fun ppf s -> incr real_sort ; pp_command ppf (Pos.none (P_symbol (sort_to_p_symbol (pp s))))

let induc_to_p_inductive : sort * symbol list -> p_inductive = fun (sort, s_l) ->
  (* p_ident * p_term * (p_ident * p_term) list *)
  let f s = (Pos.none (get_name s), sym_curry s) in
  Pos.none (Pos.none sort, Pos.none P_Type, List.map f s_l)

let pp_induc : Format.formatter -> sort * symbol list -> unit =
  fun ppf i -> incr real_induc ; pp_command ppf (Pos.none (P_inductive([], [], [induc_to_p_inductive i])))

let pp_symbol : Format.formatter -> symbol * attribute list -> unit =
  fun ppf ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr real_symbol ;
  pp_command ppf (Pos.none (P_symbol (symbol_to_p_symbol s attr_l)))

type output_management = K | Kore | Dedukti

let k_format = ref false
let kore_format = ref true
let dk_format = ref false

let dk_output = ref false
let lp_output = ref true

let set_format o = if o = "K" || o = "k" then k_format := true
                   else
                     if o = "Kore" || o = "kore" then kore_format := true
                     else
                       if o = "Dedukti" || o = "dedukti" then dk_format := true
                       else failwith ("The option" ^ o ^ "is unknow")

let set_output o = if o = "Dedukti" || o = "dedukti" then dk_output := true
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
      input := open_in s;
      Format.printf "%s" usage_msg) "compiles a .kore program"

let () =
  let lexbuf = Lexing.from_channel (!input) in
  let file = Kparser.file Klexer.token lexbuf in
  (*let rec print_c c = match c with
    | [] -> Format.fprintf Format.std_formatter "\n"
    | a::t -> Format.fprintf Format.std_formatter "%a" pp_command a; print_c t
  in
  print_c c;*)

  (* let param_to_p_params : param -> p_params = fun p -> *)

  let trans_alias : Format.formatter -> alias * (quant_var list * axiom * attribute list) option -> unit =
    fun ppf v ->
    match v with
     | _, None -> () (* @TODO *)
     | al, Some(_,ax,_) ->
        try
          pp_command ppf (Pos.none (P_rules [create_rewriting_rule al ax])) ;
          incr real_rule
        with ConditionalRule _ -> ()
  in
  let trans_axiom : Format.formatter -> quant_var list * axiom * attribute list -> unit =
    fun ppf (qv_l, a, attr_l) ->
    match attr_l with
     | Unit _::nil | Assoc _::nil | Idem _::nil ->
        (* if is_only_assoc a then @TODO *)
        incr real_rule ; pp_command ppf (Pos.none (P_rules [of_equality_axiom a]))
     | _ -> () (* @TODO *)
  in
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
        kmodule -> name * import list * sort list * (symbol list) Induc.t * (symbol * attribute list) list *
                  (alias * (quant_var list * axiom * attribute list) option) list *
                  (quant_var list * axiom * attribute list) list =
    fun (name, i_l, c_l, _) ->
    let rec aux l ((sort_l, induc_m, sym_l, alias_l, ax_l) as acc) =
      match l with
       | [] -> acc
       | (c, attr_l)::q ->
          match c with
           | Sort   s | H_sort   s ->
              incr nb_sort ; print_new_attribute s attr_l ;
              aux q (s::sort_l, induc_m, sym_l, alias_l, ax_l)
           | Symbol s | H_symbol s ->
              begin
                let name,_,_,_ = s in print_new_attribute name attr_l ;
                if not(!check_induc) then (incr nb_symbol ; aux q (sort_l, induc_m, (s,attr_l)::sym_l, alias_l, ax_l))
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
           | Alias al->
              (match q with
                | [] -> incr nb_alias ; aux q (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)
                | h::tl ->
                   (match h with
                     | Axiom(qv,a), attr_l ->
                        if is_rule_axiom a
                        then
                          (incr nb_rule ;
                           aux tl (sort_l, induc_m, sym_l, (al, Some(qv,a,attr_l))::alias_l, ax_l))
                        else
                          (incr nb_alias ;
                           aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l))
                     | _ -> incr nb_alias ; aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)))
           | Axiom(qv,a) ->
              incr nb_axiom ;
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
  let print_info : int * int option * string * string -> unit = fun (i, j, one, several) ->
    (* Format.fprintf (colorize Format.std_formatter) "Hello!" *)
    let denomi = match j with
     | None -> "?"
     | Some x -> string_of_int x
    in
    if j = Some 0 && i = 0
    then ()
    else
      if i < 2
      then Format.printf "%i / %s %s translated.\n" i denomi one
      else Format.printf "%i / %s %s translated.\n" i denomi several
  in
  let module_to_file : kmodule -> unit = fun m ->
    (* let name, import_l, command_l, attribut_l = m in *)
    nb_import := 0 ; nb_sort := 0 ; nb_symbol := 0 ;
    nb_alias  := 0 ; nb_rule := 0 ; nb_axiom := 0 ;

    real_import := 0 ; real_sort := 0 ; real_induc := 0 ; real_symbol := 0 ;
    real_alias  := 0 ; real_rule := 0 ; real_axiom := 0 ;

    let name, kimport_l, kcommand_l, _ = m in
    let filename = (String.lowercase_ascii name) ^ ".lp" in
    Format.printf (blu "--- Translation of %s\n") filename;

    let len = List.length in
    Format.printf (red "There are %i commands\n") (len kcommand_l);

    let _, import_l, sort_l, induc_m, sym_l, alias_l, ax_l = preprocessing m in

    (* let import_l = if Induc.is_empty induc_m then import_l else ("prelude", [])::import_l in *)

    let f  = open_out filename in
    let ff = Format.formatter_of_out_channel f in

    List.iter (pp_import ff [lp_pkg]) import_l;
    pp_import ff (lp_pkg::prelude_path) (prelude_name, []);
    List.iter (pp_sort ff) sort_l;
    List.iter (pp_induc ff) (List.rev (Induc.bindings induc_m));
    List.iter (pp_symbol ff) sym_l;
    (*List.iter (trans_command ff) command_l;*)
    List.iter (trans_alias ff) alias_l;
    List.iter (trans_axiom ff) ax_l;
    (*List.iter (trans_command Format.std_formatter) command_l;*)

    let info_l = [ (!real_import, Some (len kimport_l), "import", "imports")
                 ; (!real_sort,   Some !nb_sort,   "sort", "sorts")
                 ; (!real_induc,  Some 0,          "inductive type", "inductive types")
                 ; (!real_symbol, Some !nb_symbol, "symbol", "symbols")
                 ; (!real_alias,  Some !nb_alias,  "alias", "alias")
                 ; (!real_rule,   Some !nb_rule,   "rule", "rules")
                 ; (!real_axiom,  Some !nb_axiom,  "axiom", "axioms") ]
    in
    List.iter print_info info_l;

    Format.printf "------------------------------------------------------------\n";
    Format.pp_print_flush ff ();
    close_out f
  in
  Format.printf (gre "-------------------- Welcome to Kamelo ---------------------\n");
  List.iter module_to_file (snd file);
  Format.printf (gre "------------------------------------------------------------\n");
  flush stdout;;
