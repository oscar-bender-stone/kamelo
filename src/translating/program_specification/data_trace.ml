open Common.Error
open Yojson.Basic.Util (* The JSON manipulation functions *)

module StrMap = Map.Make(String)

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

let print_step : step -> unit = fun (claim, steps) ->
  let print_kore (s : string) =
    (* string2kore step *)
    let lexbuf = Lexing.from_string s in
    let file =
      try Parsing.Kparser.file Parsing.Klexer.token lexbuf
      with e -> red_msg_1 _STDOUT "Parsing fails line %i" !Parsing.Count_line.curr_line ; raise e
    in
    (match file with
     | F_pgm(x,_) -> Printing.Kore_printer.pp_kore_axiom Format.std_formatter 2 x
     | _ -> ()) ;
    (* Newline *)
    Printf.printf "\n"
  in
  let print_step (ax, cond) =
    Printf.printf "\nNew step: " ; print_kore ax   ;
    Printf.printf "Condition: "  ; print_kore cond ;
  in
  (* Print the first step / claim *)
  print_step claim ;
  (* Print each step *)
  let f (r, l) : unit =
    Printf.printf "Rule name: %s\n" r ;
    let g (s, t) =
      Printf.printf "The substitution: \n" ;
      StrMap.iter (fun key v -> Printf.printf "    * %s |-> " key ; print_kore v) s ;
      print_step t
    in
    List.iter g l
  in
  List.iter f steps
(* Printf.fprintf stdout "Hop\n%s" (fst (fst s)) (* ; List.iter print (snd s) *) *)


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
    List.iter print_step steps
  else
    failwith "ERROR"
