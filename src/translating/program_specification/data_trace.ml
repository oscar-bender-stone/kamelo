open Common.Error
open Yojson.Basic.Util (* The JSON manipulation functions *)
open Common.Type
open Interface.Output
module StrMap = Map.Make(String)


type output = Format.formatter

let pp_endline ppc = print ppc "\n"
let pp_paren   ppc = print ppc ")"
let space ppc : unit = print ppc "  "

let pp_list ppc : string -> (output -> 'a -> unit) -> 'a list -> string -> string -> unit =
  fun first f l separator last ->
  let prints = print ppc "%s" in
  prints first ;
  let rec aux l = match l with
    | []  -> prints " "
    | [t] -> f ppc t
    | t1::t2::q -> f ppc t1 ; prints separator ; aux (t2::q)
  in
  aux l ; prints last

let pp_kore_param ppc : param -> unit = fun p -> match p with
  | S s  -> print ppc "%s" (pp s)
  | Q qv -> print ppc "%s" (pp qv)

let pp_kore_quant_var_list ppc : quant_var list -> unit = fun qv_l ->
  let f_qv ppc qv = print ppc "%s" (pp qv) in
  pp_list ppc " " f_qv qv_l " " " "

let pp_kore_param_list ppc : param list -> unit = fun p_l ->
  pp_list ppc "(" pp_kore_param p_l  "," ")"

let pp_kore_param_list_bis ppc : param list -> unit = fun p_l ->
  match p_l with
  | [] -> Printf.printf " "
  | _  -> pp_list ppc " " pp_kore_param p_l  " " " "

let rec pp_kore_axiom ppc : axiom -> unit = fun ax ->
  let prints = print ppc "%s" in
  let tmp : param list -> axiom -> axiom -> unit =
    fun p_l ax1 ax2 ->
    pp_kore_param_list ppc p_l ;
    pp_endline ppc ;
    pp_kore_axiom ppc ax1 ;
    prints ",\n" ;
    pp_kore_axiom ppc ax2
  in
  let tmp2 : param list -> name -> param -> axiom -> unit =
    fun p_l n p ax ->
    pp_kore_param_list ppc p_l ;
    pp_endline ppc ;
    print ppc "%s : " (pp n) ; pp_kore_param ppc p ;
    print ppc "%s" ",\n" ;
    pp_kore_axiom ppc ax
  in
  match ax with
  | Equals(p_l, ax1, ax2) ->
     prints "#EQUALS(" ; tmp p_l ax1 ax2 ; pp_paren ppc
  | Exists(p_l, (n,p), ax) ->
     prints "#EXISTS(" ; tmp2 p_l n p ax ; pp_paren ppc
  | And(p_l, ax1, ax2) ->
     prints "#AND(" ; tmp p_l ax1 ax2 ; pp_paren ppc
  | Or(p_l, ax1, ax2) ->
     prints "#OR(" ; tmp p_l ax1 ax2 ; pp_paren ppc
  | Not(p_l, ax) ->
     prints "#NOT(" ; pp_kore_param_list ppc p_l ;
     pp_endline ppc ;
     pp_kore_axiom ppc ax ; pp_paren ppc
  | Implies(p_l, ax1, ax2) ->
     prints "#IMPLIES(" ; tmp p_l ax1 ax2 ; pp_paren ppc
  | Bottom p_l ->
     prints "#BOTTOM" ; pp_kore_param_list_bis ppc p_l
  | Top p_l ->
     prints "#TOP" ; pp_kore_param_list_bis ppc p_l
  | Rewrites(p_l, ax1, ax2) ->
     prints "#REWRITES(" ; tmp p_l ax1 ax2 ; pp_paren ppc
  | In(p_l, (n,p), ax) ->
     prints "#IN(" ; tmp2 p_l n p ax ; pp_paren ppc
  | Dom_val(sort, n) ->
     print ppc "(#DOMAIN_VALUES %s %s)" (pp sort) (pp n)
  | Ceil(p_l, ax) ->
     prints "#CEIL(" ; pp_kore_param_list ppc p_l ;
     pp_endline ppc ;
     pp_kore_axiom ppc ax ; pp_paren ppc
  | Predicate p -> pp_kore_predicat ppc p
and pp_kore_predicat ppc p = match p with
  | Sym(n, p_l, ax_l) ->
     print ppc "(%s" (pp n) ; pp_kore_param_list_bis ppc p_l ;
     let f ppc ax = pp_kore_axiom ppc ax in
     (match ax_l with
      | [] -> ()
      | _  -> pp_list ppc "" f ax_l " " "") ; print ppc ")"
  | Var(n, _) -> print ppc "%s" (pp n)








(** [add key value m] adds the value [value] at the entry [key] in the map [m]. *)
let add : string -> 'a -> 'a StrMap.t -> 'a StrMap.t = fun key value m ->
  let f a = match a with
    | None   -> Some value   (* If the key did not yet exist *)
    | Some _ -> failwith "The variable has several values in the substitution."
  in
  StrMap.update key f m

type axiom = string
type condition = string
type rule_name = string
type substitution = string StrMap.t (* var_name |-> value *)
type state_init = axiom * condition
type state = axiom * condition

type step_with_one_rule = rule_name * (substitution * state) list
type step = state_init * step_with_one_rule list

(** To get the data from the KProver trace *)

let get_state_label label oj =
  let res_subs =
    oj |> member label |> member "substitution" |> to_list
  in
  let res_term =
    oj |> member label |> member "term"         |> to_string
  in
  let res_constraint =
    oj |> member label |> member "constraint"   |> to_string
  in
  if res_subs = [] then (res_term, res_constraint)
  else failwith ("Update [get_state_label] (Label " ^ label ^ ")")

let get_state oj =
  let res_subs       = oj |> member "substitution" |> to_list   in
  let res_term       = oj |> member "term"         |> to_string in
  let res_constraint = oj |> member "constraint"   |> to_string in
  if res_subs = [] then (res_term, res_constraint)
  else failwith "Update [get_state]"

let get_substitution oj : substitution =
  let raw_data = oj |> member "substitution" |> to_list in
  let f m oj =
    let key   = oj |> member "key"   |> to_string in
    let value = oj |> member "value" |> to_string in
    add key value m
  in
  List.fold_left f StrMap.empty raw_data
  (* : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)


let get_task           oj = oj |> member "task"    |> to_string
let get_initial_state  oj = oj |> member "initial" |> to_string
let get_steps          oj = oj |> member "steps"   |> to_list
let get_final_state    oj =
  let tmp = oj |> member "finals" |> to_list in
  match tmp with
  | []   -> failwith "No final state"
  | [h]  -> get_state h
  | _::_ -> failwith "Several final states"

let get_each_step_with_one_rule oj : step_with_one_rule =
  let get_rule_name oj : string = try oj |> member "rule-id" |> to_string with _ -> failwith "t42" in
  let get_rhs oj : step_with_one_rule =
    let tmp = oj |> member "results" |> to_list in
    match tmp with
     | []   -> failwith "No result"
     | [h]  -> get_rule_name oj, [(get_substitution oj, get_state h)]
     | _::_ -> failwith "Several results"
  in
  get_rhs oj

let get_step oj : step =
  let get_remainders oj = oj |> member "remainders" |> to_list in
  let get_lhs oj = get_state_label "initial" oj in
  if get_remainders oj = [] then
    let tmp = oj |> member "applied-rules" |> to_list in
    match tmp with
    | []   -> ( (* Printf.printf "TR\n" ; *) failwith "No rule")
    | [h]  -> (get_lhs oj, [get_each_step_with_one_rule h])
    | _::_ -> ( (* Printf.printf "RE\n" ; *) failwith "Several rules")
  else
    ((*Printf.printf "TRA\n" ; *) failwith "Need to update [get_step] - remainders")


(** To print the data from the KProver trace *)











let print_kore (s : string) with_space =
  (* string2kore step *)
  let lexbuf = Lexing.from_string s in
  let file =
    try Parsing.Kparser.file Parsing.Klexer.token lexbuf
    with e -> red_msg_1 _STDOUT "Parsing fails line %i" !Parsing.Count_line.curr_line ; raise e
  in
  (match file with
   | F_pgm(x,_) ->
     if with_space
     then Printing.Kore_printer.pp_kore_axiom Format.std_formatter 2 x
     else pp_kore_axiom Format.std_formatter x
   | _ -> ()) ;
  (* Newline *)
  if with_space then Printf.printf "\n" else ()

let print_step : step -> unit = fun (claim, steps) ->
  let print_step (ax, cond) =
    Printf.printf "@@ TERM PATTERN: " ; print_kore ax true   ;
    Printf.printf "@@ PREDICATE PATTERN: "  ; print_kore cond true ;
  in
  Printf.printf "\n ------------- New step ------------- \n" ;
  Printf.printf "@ PATTERN BEFORE APPLYING THE RULE:\n" ;
  (* Print the first step / claim *)
  print_step claim ;
  (* Print each step *)
  let f (r, l) : unit =
    Printf.printf "@ RULE NAME: %s\n" r ;
    let g (s, t) =
      Printf.printf "@ THE SUBSTITUTION:\n" ;
      StrMap.iter (fun key v -> Printf.printf "    * %s |-> " key ; print_kore v true) s ;
      Printf.printf "@ PATTERN AFTER APPLYING THE RULE:\n" ;
      print_step t
    in
    List.iter g l
  in
  List.iter f steps

(* Printf.fprintf stdout "Hop\n%s" (fst (fst s)) (* ; List.iter print (snd s) *) *)

(** To use the data from the KProver trace *)

(*    Printf.printf "symbol %s : Prf (  )" rule_name

       *)




let get_KProver_proof_objects () =
  let s = !Terminal.Cmd_line.trace_file in
  let json_file = (* .yml -> .json *)
    try
      (String.sub s 0 ((String.length s)-4)) ^ ".json"
    with _ -> Printf.printf "%s\n" s ; ""
  in
  let res = Yaml_unix.of_file_exn Fpath.(v !Terminal.Cmd_line.trace_file) in
  let chan = (open_out json_file) in
  Ezjsonm.value_to_channel chan res (* true stdout (res true) *) ;
(* Yaml_unix.to_file_exn Fpath.(v "res.json") res *)

  close_out chan ;

    (* Yaml.to_json (`A [res]) *)
  let objectJSON = Yojson.Basic.from_file json_file in

  if get_task objectJSON = "reachability" then
    (* let phi_init = try get_initial_state objectJSON with _ -> failwith "Phi init" in *)
    let steps = try List.map get_step (get_steps objectJSON) with _ -> failwith "steps" in
    (* let phi_final = try get_final_state objectJSON with _ -> failwith "Phi final" in
    (phi_init, steps, phi_final) *)
    List.iter print_step steps ;

    Printf.printf "\nPROOF STEP\n" ;
    let build_proof : step -> unit = fun (_, steps) ->
      let used_rule (rule_name, l) : unit =
        let used_substitution (substitution, _) =
          let f _ value =
            Printf.printf "\n   " ; print_kore value false
          in
          StrMap.iter f substitution ;
        in
        Printf.printf "%s " rule_name ; List.iter used_substitution l ;
        Printf.printf "\n"
      in
      List.iter used_rule steps
    in
    List.iter build_proof steps
  else
    failwith "ERROR"
