
(** This file generates the variante of Viry's transformation
    from Conditional Term Rewriting System (CTRS) to Term
    Rewriting System (TRS).  *)

(** Some definitions:
      - $Var(t)$ l'ensemble des variables apparaissant dans le terme $t$
      - Soit $\Sigma$ une signature que nous partitionnons en 2 ensembles disjoints,
            * $\mathcal{C}$, l'ensemble des constructeurs, et
            * $\mathcal{D}$, l'ensemble des symboles définis.
      - Un terme $t$ uniquement composé de variables et de constructeurs est dit être un terme constructeur.
      - Un terme $f(t_1,...,t_n)$ $(n \ge 0)$ est un pattern si $f \in \mathcal{D}$ et si $t_1,...,t_n$ sont des termes constructeurs.
      - Un système de réécriture $\mathcal{R}$ est dit basé sur les constructeurs (constructor-based) si pour toute règle de réécriture $l \hookrightarrow r \in \mathcal{R}$,
            * $l$ et $r$ ont le même type,
            * $l$ est un pattern
            * $Var(r) \subseteq Var(l)$.


%	\Krule max X Y     => Y requires X <_{Int} Y
%	\Krule max X (S Y) => X requires X >_{Int} Y
%	\Krule max X 0     => X requires X \ge_{Int} 0

Considérons le système suivant
(1) \Krule max X Y => Y requires X <_{Int} Y
(2) \Krule max X Y => X requires X \ge_{Int} Y
L'encodage permet d'obtenir le système de réécriture suivant dans Dedukti :
(0)   rule max $x $y --> ♭carrier2 (max $x $y) ♭ ♭
(1')  rule ♭carrier2 (max $x $y) ♭    _ --> ♭carrier2 (max $x $y) ($x < $y) _
(1'') rule ♭carrier2 (max $x $y) true _ --> $y
(2')  rule ♭carrier2 (max $x $y) _ ♭    --> ♭carrier2 (max $x $y) _ ($x \ge $y)
(2'') rule ♭carrier2 (max $x $y) _ true --> $x

L'idée générale de l'encodage, proposé dans cette section et initialement proposée par Viry~\cite{viry1999},
est de stocker dans les arguments d'un symbole, les résultats (pouvant être partiels) de l'évaluation des conditions.
La règle (0) encapsule le calcul en cours à l'aide du symbole $\flat carrier2$, où tous les arguments booléens valent $\flat$,
symbole correspondant à $\bot$ dans l'article de Viry, et indiquant que les arguments booléens n'ont pas encore été initialisés par une condition.
Les règles (1') et (2') initialisent les conditions à calculer, tandis que
les règles (1'') et (2'') réduisent la taille du terme puisqu'une des conditions a été évaluée à $true$.

Contrairement à Viry, nous choisissons d'étendre la signature, comme ici avec le symbole $\flat carrier2$,
plutôt que de remplacer chaque symbole de la signature par un symbole équivalent mais avec une arité plus grande,
permettant de transporter les calculs des conditions.
En effet, cela complique le code et oblige à traduire, après coup, les formes normales obtenues.

Une variante de cet encodage~\cite{conditional_narrowing} s'intéresse au cas des CTRS dits basés sur les constructeurs,
et indique qu'il n'est pas nécessaire de garder en arguments les variables des conditions au moment de l'initialisation de celles-ci.
Ici, comme le montre l'exemple précédent, nous procédons de même.

De plus, cet encodage a l'avantage de garder un nombre de règles de réécriture assez proche du nombre de règles initial,
ainsi que de ne pas fixer l'ordre d'évaluation des conditions, mais augmente le temps de calcul.

Enfin, pour générer les règles précédentes, il faut connaître toutes les conditions qui peuvent s'appliquer, pour un symbole de tête donné.
Cependant, la quasi-totalité des règles de réécriture écrites dans \K, i.e. celles n'ayant pas l'attribut \KAanywhere, nécessitent d'inférer la configuration.
Cela implique que si nous considérons la définition usuelle d'un symbole de tête, la quasi-totalité des règles de réécriture auraient le même symbole de tête.
Dans le cadre de cet article, nous considérerons donc que le symbole de tête d'une règle de réécriture n'ayant pas l'attribut \KAanywhere correspond au symbole de tête
de la partie du LHS se trouvant dans la cellule \texttt{<k>}.
Nous notons $head_{<k>}$ la fonction qui renvoie le symbole de tête de la cellule \texttt{<k>}
\amelie{(ou de l'élément en tête de la K computation dans la cellule \texttt{<k>}) ?},
si la cellule \texttt{<k>} existe, sinon elle renvoie le symbole de tête au sens usuel.
Nous notons également $\mathcal{C}_\sigma$, l'ensemble des règles qui donne le même résultat, noté $\sigma$,
par la fonction $head_{<k>}$, i.e. $l \hookrightarrow r \in \mathcal{C}_\sigma$ si $head_{<k>}~(l \hookrightarrow r) = \sigma$.


Algorithm:

\noindent Soit $\mathfrak{R}$ un système de réécriture conditionnel écrit dans \K et supposé être basé sur les constructeurs.
Nous supposons également que $\flat$ est un nom non utilisé, même en tête de nom de tout symbole.
Nous présentons la traduction notée $||~.~||_{CTRS}$ précédemment.
Celle-ci prend en arguments un ensemble de triplets de termes \Dedukti de la forme $(LHS, RHS, Cond)$, noté $\mathcal{E}_{DK}$ et
obtenu une fois les traductions \traducKompile{.} et \traducKamelo{.} appliquées sur $\mathfrak{R}$.
Après avoir construit les $\mathcal{C}_\sigma$ à partir de $\mathcal{E}_{DK}$, nous déroulons l'algorithme suivant pour chaque $\mathcal{C}_\sigma$ :
	[1.] Calculer le nombre de conditions dans $\mathcal{C}_\sigma$, noté $X$.
	[2.] Générer le LHS le plus général pour un symbole $\sigma$ donné, noté $mglhs_\sigma$.
	[3.] Générer le symbole support ♭carrierX de type K -> \Knatif{Bool} -> ... -> \Knatif{Bool}, avec $X$ arguments de type \Knatif{Bool}.
         \amelie{Il faudrait plutôt considérer le type ♭Bool = \Knatif{Bool} \cup {♭}
	[4.] Générer la règle d'encapsulation : $mglhs_\sigma --> ♭carrierX mglhs_\sigma ♭ ... ♭, avec un ♭ pour chaque condition.
	[5.] Initialiser $i$ à 0. Si $|\mathcal{C}_\sigma| = X$, aller en 7, sinon aller en 6.
	[6.] Pour chaque règle appartenant à $\mathcal{C}_\sigma$ et n'ayant pas l'attribut \KAowise :
		[A.] Si la règle est de la forme $l \underset{c}{\hookrightarrow} r \in \mathcal{C}_\sigma$, incrémenter $i$ de $1$, puis :
			  * Générer une règle d'initialisation : ♭carrierX l _ ... _ ♭             _ ... _ --> ♭carrierX l _ ... _ c _ ... _.
			  * Générer une règle de réduction :     ♭carrierX l _ ... _ \Knatif{true} _ ... _ --> r.
		      * Remarque : ♭, $c$ et \Knatif{true} se trouvent à la position $i$
		[B.] Si la règle est de la forme l --> r \in \mathcal{C}_\sigma$, générer : ♭carrierX l _ ... _ --> r.
		     Remarque : Si $\mathcal{C}_\sigma$ ne possède pas de règles conditionnelles, nous pouvons ne rien changer.
	[7.] Si une règle l --> r \in \mathcal{C}_\sigma$ à l'attribut \KAowise, générer : ♭carrierX l \Knatif{false} ... \Knatif{false} --> r.

\amelie{+ Définition formelle de $mglhs_\sigma$}


Extension de l'encodage :

Une manière plus succincte d'écrire notre exemple est d'utiliser l'attribut \KAowise :
\Krule max X Y => Y requires X <_{Int} Y
\Krule max X Y => X [\KAowise]
Malheureusement, \K ne génère pas la condition complémentaire dans le fichier \Kore.
Pour encoder cet attribut, 2 possibilités donc s'offrent à nous :
 * implémenter un algorithme qui détermine la condition complémentaire
 * considérer que toutes les conditions se réduisent nécessairement soit à \Knatif{true}, soit à \Knatif{false}.
Comme nous ne connaissons pas exactement l'expressivité des conditions pouvant être écrites dans \K, outre qu'elles sont de type \Knatif{Bool},
nous préférons ajouter l'hypothèse suivante : toute fonction retournant un booléen est une fonction totale.
Sous cette hypothèse, nous pouvons générer la règle présentée en 7.
% suivante : $\sigma $x_1 ... $x_n --> \sigma $x_1 ... $x_n false ... false.

\noindent Par la suite, nous comptons étendre encore l'encodage afin de traduire l'attribut \KApriority{\textit{number}}.

 *)




open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude
open Axiom

(** Generating of conditional rewriting rule *)

(** Consider the following rewriting system:
  - [<top> (<k> (maxInt $X $Y)) ↪ <top> (<k> $X) requires X >= Y],
  - [<top> (<k> (maxInt $X $Y)) ↪ <top> (<k> $Y) requires X < Y] and
  - [<top> (<k> (add (S n) (S m))) ↪ <top> (<k> (add n (S (S m)))) ].

 It's possible to generate the following tree according to the previous
 system:
    {v                                          ├─(<top> (<k> $X), X >= Y)
                     ├─maxInt─∘─$X─∘─$Y─∘─)─∘─)─↪
 ∘─<top>─∘─(─∘─<k>─(─∘                          └─(<top> (<k> $Y), X < Y)
                     └─add─∘─(─∘─S─∘─$n─∘─)─∘─(─∘─S─∘─$m─∘─)─↪─ ...
    v}
 Now, we deduce:
  - rule <top> (<k> maxInt($X, $Y)) =>
              (X >= Y, X < Y, <top> (<k> maxInt($X, $Y)))
  - rule (true, _, <top> (<k> maxInt($X, $Y))) => <top> (<k> $X)
  - rule (_, true, <top> (<k> maxInt($X, $Y))) => <top> (<k> $Y)

  - rule <top> (<k> (add (S $n) (S $m))) ↪ ... (no change)

 To simplify:
  - private constant symbol gen3_HASH : bool -> bool -> SortK -> TYPE
  - rule <top> (<k> maxInt($X, $Y)) =>
             gen3_HASH (X >= Y) (X < Y) (<top> (<k> maxInt($X, $Y)))
  - rule gen3_HASH true _ (<top> (<k> maxInt($X, $Y))) => <top> (<k> $X)
  - rule gen3_HASH _ true (<top> (<k> maxInt($X, $Y))) => <top> (<k> $Y)

  - rule <top> (<k> (add (S $n) (S $m))) ↪ ... (no change)

 Que faire avec la règle <top> (<k> (maxInt $X (S $Y))) ↪ ... ?
*)

(** A supposed safe prefix, i.e. there is no name beginning with it. *)
let safe_prefix = "♭"

(** Type of a conditional rule, which has the form
    ((LHS, RHS), condition, priority) *)
type cond_rule = p_rule * p_term * int

(** Type of a unconditional rule, which has the form
    ((LHS, RHS), priority) *)
type uncond_rule = p_rule * int

(** Type of equivalence class related to the head symbol named σ,
    i.e. a map where each entry has the form
    σ |-> [((LHS, RHS), Some condition, priority) ; ...].
    For a head symbol σ given, C_σ is the corresponding equivalence class. *)
type equiv_class = (ctrs_rule list) StrMap.t

(** ***************** To iterate on a configuration ********************* *)

(** [is_cell s] returns if a string [s] is a cell's name. *)
let is_cell : string -> bool = fun s ->
  Format.fprintf (Format.formatter_of_out_channel stdout) "boug" ;
  let len = String.length s in
  try
    s.[0] = 'L' && s.[1] = 'b' && s.[2] = 'l' && s.[3] = '\''
    && s.[4] = '-' && s.[5] = 'L' && s.[6] = 'T' && s.[7] = '-'
    && s.[8] = '\'' && s.[len-6] = '\'' && s.[len-5] = '-' && s.[len-4] = 'G'
    && s.[len-3] = 'T' && s.[len-2] = '-' && s.[len-1] = '\''
  with _ -> Format.fprintf (Format.formatter_of_out_channel stdout) "boug" ; false

(** [is_k_cell s] returns if a string [s] is the cell's name
    of the cell k. *)
let is_k_cell : string -> bool = fun s ->
  let res = (s = _K_CELL) in
  Format.fprintf (Format.formatter_of_out_channel stdout) "%b" res ; res

let is_to_keep : string -> bool = fun s ->
  s = _KSEQ || s = _DOTK || s = _INJ

(** [get_name i] gives the short name of a p_qident [i]. *)
let get_name : p_qident -> string = fun i ->
  let (_, name) = i.elt in name

exception KCellNotFound
exception KCellNotFoundHere

let rec has_infered_configuration : p_term -> bool = fun t ->
  match t.elt with
  | P_Appl(t,_) -> has_infered_configuration t
  | P_Iden(name,_) -> is_cell (snd name.elt)
  | _ -> false

let create_most_general_LHS t =
  let nb = ref 0 in
  let new_var nb =
    incr nb ; create_pattern_var ("x" ^ string_of_int !nb)
  in
  let rec aux : p_term -> bool -> bool -> bool * bool * bool * p_term = fun t is_in_k_cell is_head ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let l_is_in_k_cell, l_merged, l_is_head, x1 = aux t1 is_in_k_cell is_head in
        if l_merged then is_in_k_cell, l_merged, l_is_head, new_var nb
        else
          let r_is_in_k_cell, r_merged, r_is_head, x2 = aux t2 l_is_in_k_cell l_is_head in
          let res =
            if r_merged then no_pos (P_Appl(x1, new_var nb))
            else no_pos (P_Appl(x1, x2))
          in
          is_in_k_cell, l_merged && r_merged, l_is_head || r_is_head, res)
    | P_Patt _ as t -> is_in_k_cell, true, is_head, no_pos t
    | P_Expl _ as t -> is_in_k_cell, not(is_in_k_cell), is_head, no_pos t
    | P_Iden (name, _) as t ->
       let n = snd name.elt in
       if is_to_keep n then
         is_in_k_cell, not(is_in_k_cell), is_head, no_pos t
       else
         (if is_cell n then
            (is_k_cell n), false, is_head, no_pos t
          else
            if is_head
            then is_in_k_cell, true,  is_head, no_pos t
            else is_in_k_cell, false, true, no_pos t)
    | _ -> failwith "ERROR"
  in
  let _,_,_,res = aux t (not(has_infered_configuration t)) false in res


let update_config : string -> p_term -> (p_term -> p_term) -> p_term = fun head config f ->
  let rec aux : bool -> p_term -> bool * p_term = fun is_head t ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let l_is_head, x1 = aux is_head t1 in
        let r_is_head, x2 = aux is_head t2 in
        if r_is_head then
          (if l_is_head
           then failwith "Several head symbols..."
           else false,  no_pos (P_Appl(x1, f x2)))
        else l_is_head, no_pos (P_Appl(x1, x2)))
    | P_Patt _ | P_Expl _ -> false, t
    | P_Iden(({elt=(x1,n);pos=y}), x2) ->
       if n = head
       then true,  no_pos (P_Iden(({elt=(x1, safe_prefix ^ n);pos=y}), x2))
       else false, t
    | _ -> failwith "ERROR"
  in
  snd(aux false config)

                        (*
(** [configuration_iter] is a function with the following arguments:
      - [config] is the configuration with which the iterator works.
      - [join_res t1 t2] joins 2 results at a @-@-@-node.
      - [aia_cell_root t1 t2] does something at a @-id-@-node,
           when the current root is a cell.
      - [aia_not_cell_root nb] does something at a @-id-@-node,
           when the current root isn't a cell.
      - [aii_cell_root t1 t2 nb] does something at a @-id-id-node,
           when the current root is a cell.
      - [aii_not_cell_root t1 t2] does something at a @-id-id-node,
           when the current root isn't a cell.
      - [aai_node nb] does something at a @-@-id-node.
      - []
    Note: @-id-@-node means P_Appl(P_Iden _, P_Appl _). *)
let configuration_iter (nb : int ref) (config : p_term)
      (* (join_res : 'a -> 'a -> 'a)
      (aia_cell_root : p_term -> 'a -> 'a)
      (aia_not_cell_root : int ref -> 'a)   *)
      (* (aii_cell_root : 'a -> p_term -> int ref -> 'a) *)
      (* (aii_not_cell_root : p_term -> p_term -> 'a)
      (aai_node : int ref -> 'a) *)
      (aop_node : p_term -> int ref -> 'a)
      (* (no_config : p_term -> int -> int ref -> 'a) *) : 'a =
  let aux : p_term -> 'a = fun t -> match t.elt with
    | P_Appl(({elt=P_Appl _ ; _} as t1), ({elt=P_Appl _ ; _} as t2)) ->
       join_res (aux t1) (aux t2)
    | P_Appl(({elt=P_Iden(name, _); _} as i), ({elt=P_Appl _; _} as t))  ->
       if is_cell (snd name.elt) || is_k_computation_constructor (snd name.elt)
       then aia_cell_root i (aux t)
       else aia_not_cell_root nb
    | P_Appl(({elt=P_Iden(name1,_); _} as i1), ({elt=P_Iden(name2,_); _} as i2)) ->
       let ff = (Format.formatter_of_out_channel stdout) in
       Format.fprintf ff "Rds : %s" (snd name1.elt) ;
       if is_cell (snd name1.elt) || is_k_computation_constructor (snd name1.elt)
       then
         if is_cell (snd name2.elt) || is_k_computation_constructor (snd name2.elt)
         then failwith "Badly nested"
         else aii_cell_root i1 i2 nb
       else aii_not_cell_root i1 i2
    | P_Appl({elt=P_Appl _ ; _},  {elt=P_Iden _; _}) -> aai_node nb
    | P_Appl (({elt=P_Appl _ ; _} as t), ({elt=P_Patt _; _}) ) ->  aii_cell_root (aux t) t nb
    | P_Appl (({elt=P_Iden _ ; _} as t), ({elt=P_Patt _; _} as p) ) -> aii_cell_root t p nb
    | P_Appl (({elt=P_Patt _ ; _}), ({elt=P_Patt _; _}) ) -> failwith "Strange?"
    | P_Appl( t,                  {elt=P_Patt _; _}) -> aop_node t nb
    (*| P_Type   -> failwith "ERROR TYPE"
    | P_Iden _ -> failwith "ERROR Iden"
    | P_Wild   -> failwith "ERROR _"
    | P_Meta _ -> failwith "ERROR Meta-var"
    | P_Patt _ -> failwith "ERROR Pattern"
    | P_Appl({elt=P_Patt _;_},_) -> failwith "ERROR Appl."
    | P_Appl(_,{elt=P_Type;_}) -> failwith "ERROR Appl right."
    | P_Appl(_,_) -> failwith "ERROR Appl other."
    | P_Arro _ -> failwith "ERROR Arrow"
    | P_Abst _ -> failwith "ERROR Abst"
    | P_Prod _ -> failwith "ERROR Prod"
    | P_LLet _ -> failwith "ERROR Let-in"
    | P_NLit _ -> failwith "ERROR Nat"
    | P_Wrap _ -> failwith "ERROR Wrap"
    | P_Expl _ -> failwith "ERROR Expl" *)
    | _ -> failwith "ERROR"
  in
  (*let rec truc : int -> p_term -> 'a = fun nb_arg t ->
    match t.elt with
    | P_Appl(({elt=P_Iden(name, _); _} as i), _)  ->
       if is_cell (snd name.elt) || is_k_computation_constructor (snd name.elt)
       then aux config
       else no_config i nb_arg nb
    | P_Appl(({elt=P_Appl _; _} as t), _)  ->
       truc (nb_arg+1) t
    | _ -> failwith "Hum?"
  in
  truc 0 config
   *)
  aux config

(** ***** To create each C_σ from a CTRS ***** *)
let get_head_symbol nb config = (* 'a = term option *)
  (*let join t1 t2 = match (t1, t2) with
    | None, None -> None
    | Some x, None -> Some x
    | None, Some x -> Some x
    | Some _, Some _ -> failwith "More than one k cell."
  in
  let f2 (t:p_term) res =
    match t.elt with
    | P_Iden(i,_) -> if is_k_cell (snd i.elt) then res else None
    | _ -> failwith "Internal bug"
  in
  let f3 (t1 : p_term) (t2 : p_term) _ =
    match t1.elt with
    | P_Iden(i,_) -> if is_k_cell (snd i.elt) then Some t2 else None
    | _ -> failwith "Internal bug"
  in
  let f4 (t : p_term) _ _ =
    match t.elt with
    | P_Iden(i,_) -> if is_k_cell (snd i.elt) then Some t else None
    | _ -> failwith "Internal bug"
  in
  let none1 : int ref -> p_term option = fun _ -> None in
  let none2 : p_term -> p_term -> p_term option = fun _ _ -> None in *)
  let none2i : p_term -> int ref -> p_term option = fun _ _ -> None in
  configuration_iter nb config (* join f2 none1 f3  none2 none1 *) none2i (* f4 *)
                         *)
(** [get_head_symbol _ config] -> None si pas de configuration / Some _ sinon. *)
let get_head_symbol _ config =
  let rec aux : p_term -> p_term option = fun t ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let res = aux t1 in
        match res with
        | None   -> aux t2
        | Some x -> Some x)
    | P_Patt _ -> None
    | P_Expl _ -> None
    | P_Iden (name, _) as t ->
       let n = snd name.elt in
       if is_to_keep n then
         None
       else
         (if is_cell n then None else Some (no_pos t))
    | _ -> failwith "ERROR"
  in
  aux config

(** [find_equiv_class ec t] adds the p_term [t] into the equivalence class [ec].  *)
let find_equiv_class : equiv_class -> ctrs_rule -> equiv_class =
  fun ec (({elt=(lhs,_);_},_,_) as r) ->
  let key = match get_head_symbol (ref 0) lhs with
    | None -> "hum"   (* raise  KCellNotFound *)
    | Some {elt=(P_Iden({elt=(_,x);_},_));_} ->  (* Format.fprintf ff "Bon %s\n" x ; *) x
    | _ -> failwith "Internal error"
  in
  (*
    update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    update key f m returns a map containing the same bindings as m, except for the binding of key.
    Depending on the value of y where y is f (find_opt key m), the binding of key is added, removed or updated.
    If y is None, the binding is removed if it exists;     otherwise,
    if y is Some z then key is associated to z in the resulting map.
    If key was already bound in m to a value that is physically equal to z,
    m is returned unchanged (the result of the function is then physically equal to m).
   *)
  let f a = match a with
    | None   -> Some [r]   (* Si l'entrée n'existait pas encore *)
    | Some l -> Some(r::l) (* Si l'entrée existait déjà *)
  in
  StrMap.update key f ec

(** [to_equiv_class rule_l] generates each equivalence class from
    a CTRS [rule_l], i.e. each C_σ. *)
let to_equiv_class : ctrs_rule list -> equiv_class = fun rule_l ->
  List.fold_left find_equiv_class StrMap.empty rule_l

  (*
(** ***** To create the most general LHS from σ ***** *)
let create_most_general_LHS config =
  let nb = ref 0 in
  (* let join_app t1 t2 = no_pos (P_Appl(t1, t2)) in *)
  let new_var nb =
    incr nb ; create_pattern_var ("x" ^ string_of_int !nb)
  in
  let special_join_app t _ nb = no_pos (P_Appl(t, new_var nb)) in
  (* let iter_special_join_app i t nb =
    let rec aux i (acc : p_term) =
      if i = 0 then acc
      else (incr nb ; aux (i-1) (no_pos (P_Appl(acc, new_var nb))))
    in
    aux i t (* FIX nb-1 ? *)
  in *)
  configuration_iter nb config (* join_app join_app new_var special_join_app join_app new_var *)
  (fun t nb -> special_join_app t nb nb)
  (* (fun t nb_arg nb -> iter_special_join_app nb_arg t nb) *)
  *)


(* VERSION 1



(** [add_rule map ((lhs, rhs), c, p)] *)
let add_rule : equiv_class -> ctrs_rule -> equiv_class = fun map ((lhs, rhs), c, p) as r ->
  (* STEP 0: Find the head symbol of the current rule *)
  let rec get_head_symbol : p_term -> string = fun lhs_fragment ->
    match lhs_fragment with
    | P_Appl(P_Appl(_,_) as t,_) -> get_head_symbol t
    | P_Appl(P_Iden(i, false),_) ->
       let name = get_name i in
       if is_cell name then get_k_head_symbol lhs else name
  in
  let rec get_next_head_symbol : p_term -> string = fun lhs_fragment ->
    match lhs_fragment with
    | P_Appl(P_Appl(_,_) as t,_) -> get_head_symbol t
    | P_Appl(P_Iden(i, false),_) -> get_name i
    | _ -> failwith "Strange head symbol"
  in
  (* STEP 1:  *)
  let rec get_k_head_symbol : p_term -> string = fun lhs_fragment ->
    match lhs_fragment.elt with
    | P_Appl(P_Appl(_,_) as t1, P_Appl(_,_) as t2) ->
       try get_k_head_symbol t1
       with _ ->
             try get_k_head_symbol t2
             with _ -> failwith "No K cell."
    | P_Appl(P_Appl(_,_) as t, P_Iden(i, false)) ->
    (* Cas pas possible je pense *)
       get_k_head_symbol t
    | P_Appl(P_Iden(i, false), P_Appl(t1,_) as t) ->
       let name = get_name i in
       if is_cell name then
         if is_k_cell name then get_next_head_symbol t1
         else get_k_head_symbol t
       else failwith "No K cell"
    | P_Appl(P_Iden(i1, false), P_Iden(i2, false)) ->
       let name1 = get_name i1 in
       let name2 = get_name i2 in
       if is_k_cell name1 then get_name i2
       else failwith "K cell isn't here."
                     (*
    | P_Appl(P_Iden(i1, false), P_Appl(P_Iden(i2, false), t)) ->
       let name1 = get_name i1 in
       let name2 = get_name i2 in
       if is_cell name1 then
         if is_k_cell name1 then get_name i2
         else
           if is_cell name2 then get_k_head_symbol t
           else failwith "K cell isn't here."
       else name1
                      *)
    | P_Wrap t -> get_k_head_symbol t
    | P_Iden(_,_) | P_Meta _ | P_Arro _
      | P_Abst _ | P_Prod _ | P_LLet _ | P_Expl _
      | P_Type | P_Wild | P_NLit _ | P_Patt(_,_) ->
       failwith "Configuration not well-formed."
  in
  let find_equiv_class : equiv_class -> p_term -> equiv_class = fun ec t ->
    let key = get_head_symbol t in
(*
  update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  update key f m returns a map containing the same bindings as m, except for the binding of key.
  Depending on the value of y where y is f (find_opt key m), the binding of key is added, removed or updated.
  If y is None, the binding is removed if it exists;     otherwise,
  if y is Some z then key is associated to z in the resulting map.
  If key was already bound in m to a value that is physically equal to z,
  m is returned unchanged (the result of the function is then physically equal to m).
 *)
    let f a = match a with
      | None   -> Some [r]  (* Si l'entrée n'existait pas encore *)
      | Some l -> Some r::l (* Si l'entrée existait déjà *)
    in
    SMap.update t f ec
  in
  find_equiv_class ec lhs

(** [to_equiv_class rule_l] generates each equivalence class from
    a CTRS [rule_l], i.e. each C_σ. *)
let to_equiv_class : ctrs_rule list -> equiv_class = fun rule_l ->
  List.fold_left add_rule SMap.empty rule_l

 *)

(** ******************************************************************** *)


(** *********** To create rewriting rules or symbol types ************** *)

(** Basic functions ************* *)

(** [create_list n default_sym i special_sym] creates a list with
    [n] symbols [default_sym], except the [i]e which is [special_sym].
    More precisely:
     - 0 <= n ==> List.length @result = n
     - n <= 0 ==> List.length @result = 0
     - \forall k. k <> i /\ 0 <= k < n ==> @result.[k] = default_sym
     - 0 <= i < n ==> @result.[i] = special_sym
     - Remark: n <= i ==> (\forall k. 0 <= k < n ==> @result.[k] = default_sym) *)
let create_list n default_sym i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym = if current = i then special_sym else default_sym in
      if current = (n-1) then
        [adding_sym]
      else
        adding_sym::(aux (current+1))
    in
    aux 0

(** [create_list_iter nb default_sym] creates a list, thanks to [create_list],
    with [nb] symbols [default_sym]. *)
let create_list_iter nb default_sym =
  (* nb < nb+1, so the list has only [default_sym] element *)
  create_list nb default_sym (nb+1) default_sym

(** [with_one_diff_value heading nb i special_sym] creates left- or right-hand-side
    of the form: [heading] _ ... _ [special_sym] _ ... _
    where [heading] has [nb] argument(s),
    and [special_sym] are only at the position [i]. *)
let with_one_diff_value heading nb i special_sym =
  List.fold_left create_appl heading
    (create_list nb (no_pos P_Wild) i special_sym)

(** [with_all_same_value heading nb default_sym] creates left- or right-hand-side
    of the form: [heading] [default_sym] ... [default_sym]
    where [heading] has [nb] argument(s). *)
let with_all_same_value heading nb default_sym =
  List.fold_left create_appl heading (create_list_iter nb default_sym)

(** Specific functions ************* *)

(** [create_carrier_symbol_type nb] generates the type:
    [K] -> [♭Bool] -> ... -> [♭Bool] -> [K], which has [nb+1] arguments.
    Note: ♭Bool = {♭, "true", "false"}. *)
let create_carrier_symbol_type nb =
  let injK = create_ident _INJ in
  let flatBool_type = create_appl injK (create_ident (safe_prefix ^ "Bool")) in
  let cell_type = create_appl injK (create_ident "SortGeneratedTopCell") in
  List.fold_right create_arrow (cell_type::(create_list_iter nb flatBool_type)) cell_type

(** [create_encapsulation_rule mglhs carrier_sym nb]
    creates an encapsulation rule, i.e. a rule of the form:
    rule [mglhs] --> [carrier_sym] [mglhs] ♭ ... ♭
    where [nb] occurrence(s) of ♭. *)
let create_encapsulation_rule mglhs carrier_sym nb =
  let heading = no_pos (P_Appl (create_ident carrier_sym, mglhs)) in
  (mglhs, with_all_same_value heading nb (create_ident safe_prefix))

let create_list_number n i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym =
        if current = i then special_sym
        else create_pattern_var ("y" ^ (string_of_int i)) in
      if current = (n-1) then [adding_sym] else adding_sym::(aux (current+1))
    in
    aux 0

(** [create_initialisation_rule carrier_sym lhs nb i special_sym_l special_sym_r]
    creates an initialisation rule, i.e. a rule of the form:
    rule [carrier_sym] [lhs] _ ... _ [special_sym_l] _ ... _ -->
         [carrier_sym] [lhs] _ ... _ [special_sym_r] _ ... _
    where [carrier_sym] [lhs] has [nb] argument(s),
    and [special_sym_l] and [special_sym_r] only occur at the position [i]. *)
let create_initialisation_rule carrier_sym lhs nb i special_sym_l special_sym_r =
  let heading = no_pos (P_Appl (create_ident carrier_sym, lhs)) in
  let f special_sym = List.fold_left create_appl heading (create_list_number nb i special_sym) in
  (f special_sym_l, f special_sym_r)

(** [create_reduction_rule carrier_sym lhs nb i special_sym rhs]
    creates a reduction rule, i.e. a rule of the form:
    rule [carrier_sym] [lhs] _ ... _ [special_sym] _ ... _ --> [rhs]
    where [carrier_sym] [lhs] has [nb] argument(s),
    and [special_sym] only occurs at the position [i]. *)
let create_reduction_rule carrier_sym lhs nb i special_sym rhs =
  let heading = no_pos (P_Appl (create_ident carrier_sym, lhs)) in
  (with_one_diff_value heading nb i special_sym, rhs)

(** [create_otherwise_rule carrier_sym lhs nb rhs]
    creates an otherwise rule, i.e. a rule of the form:
    rule [carrier_sym] [lhs] "false" ... "false" --> [rhs]
    where [nb] occurrence(s) of "false". *)
let create_otherwise_rule carrier_sym lhs nb rhs =
  let heading = no_pos (P_Appl (create_ident carrier_sym, lhs)) in
  (with_all_same_value heading nb (create_appl (create_ident (safe_prefix ^ _INJ)) p_FALSE), rhs)


(** *********** To create rewriting rules or symbol types ************** *)
(*
let with_one_diff_value heading nb i special_sym =
  List.fold_left create_appl heading
    (create_list nb (no_pos P_Wild) i special_sym)

let with_all_same_value heading nb default_sym =
  List.fold_left create_appl heading (create_list_iter nb default_sym)
  *)

(** Specific functions ************* *)

(** [create_carrier_symbol_type nb] generates the type:
    [K] -> [♭Bool] -> ... -> [♭Bool] -> [K], which has [nb+1] arguments.
    Note: ♭Bool = {♭, "true", "false"}. *)
let extend_type typ nb =
  let split_type : p_term -> p_term list * p_term = fun typ ->
    let rec aux (t : p_term) acc = match t.elt with
      | P_Type   -> [], t
      | P_Iden _ -> [], t
      | P_Arro(t1, ({elt=P_Type  ;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Iden _;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Arro _;_} as t2)) -> aux t2 (t1::acc)
      | _ -> failwith "Bad signature"
    in
    aux typ []
  in
  let flatBool_type = create_appl p_INJ (create_ident (safe_prefix ^ "Bool")) in
  let arg_type, output_type = split_type typ in
  List.fold_right create_arrow (arg_type@(create_list_iter nb flatBool_type)) output_type

let create_encapsulation_rule_bis tracker config nb =
  let f h = with_all_same_value h nb (create_ident safe_prefix) in
  (config, tracker config f)

let create_list_number n i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym =
        if current = i then special_sym
        else create_pattern_var ("y" ^ (string_of_int i)) in
      if current = (n-1) then [adding_sym] else adding_sym::(aux (current+1))
    in
    aux 0

let create_initialisation_rule_bis tracker nb i special_sym_l special_sym_r =
  let f special_sym h = List.fold_left create_appl h (create_list_number nb i special_sym) in
  (tracker (f special_sym_l), tracker (f special_sym_r))

let create_reduction_rule_bis tracker nb i special_sym rhs =
  let f h = with_one_diff_value h nb i special_sym in
  (tracker f, rhs)

let create_otherwise_rule_bis tracker nb rhs =
 let f h = with_all_same_value h nb (create_appl (create_ident (safe_prefix ^ _INJ)) p_FALSE) in
 (tracker f, rhs)

(** ------------------------------------------------------------------------------------------------- *)



(** ------------------------------------------------------------------------------------------------- *)
(*
let generate_rule : p_term -> krule list -> p_rule list -> p_rule list = fun key krule_l acc ->
  (* STEP 0: Count the number of conditions *)
  let rec count_condition : krule_l -> int -> int = fun l acc ->
    match l with
    | [] -> acc
    | (_, None,   _)::q -> count_condition q acc
    | (_, Some _, _)::q -> count_condition q (acc+1)
  in
  let nb_cond = count_condition krule_l 0 in
  (* STEP 1: Generate the gluing symbol *)
  let create_gluing_symbol : p_symbol -> int -> p_symbol = fun sym nb_cond ->
    let mod_l = [] in
    let fresh_name = "♭" ^ sym.sym_name in
    let typ = extend_type (sym.sym_typ) nb_cond in
    LP_p_term.create_p_symbol mod_l fresh_name [] typ None
  in
  let gluing_sym = create_gluing_symbol key nb_cond in (* p_term doit devenir p_symbol !! *)
  (* STEP 2: Create the initializer rule *)
  let gluing = change_lhs gluing_sym lhs in

  let generate : krule -> p_rule list = fun kr ->
    let rec aux l acc_rhs acc =
      match l with
      | ((lhs,rhs), None,   _)::q -> aux q _ ((lhs, rhs)::acc)
      | ((lhs,rhs), Some c, _)::q -> aux q (P_Appl(acc_rhs,c))
    in
    aux lhs []

  let generate_all_rule : equiv_class -> p_rule list = fun ec ->
    TMap.fold generate_rule ec []
  in

  (* STEP 3: Create each terminal rules *)
  let rec terminal_rule : int -> p_rule list -> p_rule list = fun indice acc ->
    let rec aux indice l acc =
      match l with
      | ((lhs,rhs), None,   _)::q -> aux indice q ((lhs, rhs)::acc)
      | ((lhs,rhs), Some c, _)::q ->
         let new_rule = create_terminal_true_rule gluing indice nb_cond, rhs in
         aux (indice+1) q (new_rule::acc)
    in
    aux 0 l []
  in

    if indice = nb_cond then acc
    else
      let new_rule = create_terminal_true_rule gluing indice nb_cond, ??? in
      terminal_rule (indice+1) (new_rule::acc)
  in
  terminal_rule 0 []

    type krule = p_rule * p_term option * int    (** ((LHS, RHS), Some condition, priority)          *)
                 *)
(** ------------------------------------------------------------------------------------------------- *)


(*  type equiv_class = (ctrs_rule list) SMap.t *)

let viry_encoding : ctrs_rule list -> p_symbol list * p_rule list = fun l ->
  (* [0.] Create the initial data (♭Bool, ♭, ♭inj, and each C_σ). *)
     (* [a.] Create the symbol ♭Bool. *)
  let flat_bool_sym = Interface.LP_p_term.create_p_symbol [] (safe_prefix ^ "Bool") [] (Some p_SORTK) None in
     (* [b.] Create the symbol ♭. *)
  let flat_type = create_appl (create_ident _INJD) (create_ident (safe_prefix ^ "Bool")) in (* _INJD ♭Bool *)
  let flat_sym = Interface.LP_p_term.create_p_symbol [] safe_prefix [] (Some flat_type) None in
     (* [c.] Create the symbol ♭inj. *)
  let flat_inj_type = (* _INJD SortBool → _INJD ♭Bool *)
    create_arrow
      (create_appl p_INJD (create_ident "SortBool"))
      (create_appl p_INJD (create_ident (safe_prefix ^ "Bool")))
  in
  let flat_inj_sym = Interface.LP_p_term.create_p_symbol [] (safe_prefix ^ _INJ) [] (Some flat_inj_type) None in
     (* [d]. Create each C_σ from a CTRS. *)
  let equiv_class = to_equiv_class l in
  (* For each C_σ *)
  let aux_sigma : string -> ctrs_rule list -> p_symbol list * p_rule list -> p_symbol list * p_rule list =
    fun head_name l (acc_sym, acc_rule) ->
    let tracker = update_config head_name in
    (* [1.] Compute the number of conditions in C_σ, in order to know
            if there is no conditional rule. *)
    let nb_cond = List.fold_left (fun acc (_,data,_) -> match data with | Cond _ -> 1 + acc | _ -> 0 + acc) 0 l in
    if nb_cond = 0
    then acc_sym, List.map (fun (pr,_,_) -> pr) l@acc_rule
    else
      (* [2.] Generate the most general LHS for a given head symbol σ. *)
      let (pr, _, _) = List.hd l in (* FIX if the list is empty *)
      let mglhs = create_most_general_LHS (fst pr.elt) in
      (* [3.] Generate the ♭-symbol of the current head symbol. *)
      let flat_head_name = safe_prefix ^ head_name in
      (* let flat_head_type = extend_type (Map.find head_name) nb_cond in @TODO *)
      let flat_head_type = p_TYPE in
      let flat_head_sym = Interface.LP_p_term.create_p_symbol [] flat_head_name [] (Some flat_head_type) None in
      (* [4.] Generate the encapsulation rule. *)
      let encap_r = no_pos (create_encapsulation_rule_bis tracker mglhs nb_cond) in
      (* [5.] For each rule in C_σ *)
      let rec aux_rule : int -> p_rule list -> ctrs_rule list -> p_rule list = (* ~ fold_lefti *)
        fun i acc ctrs_l ->
        match ctrs_l with
        | [] -> acc
        (* [a.] If the rule is conditional. *)
        | ({elt=(lhs,rhs);_}, Cond c, _)::q ->
           let curr_tracker = tracker lhs in
           let flat_cte = create_ident safe_prefix in
           let init_r =
             no_pos (create_initialisation_rule_bis curr_tracker nb_cond i flat_cte c)
           in
           let true_cte = create_appl (create_ident (safe_prefix ^ _INJ)) p_TRUE in
           let reduc_r =
             no_pos (create_reduction_rule_bis curr_tracker nb_cond i true_cte rhs)
           in
           aux_rule (i+1) (init_r::reduc_r::acc) q
        (* [b.] If the rule is unconditional. *)
        | ({elt=(lhs,rhs);_}, Uncond, _)::q ->
           let reduc_r =
             no_pos (create_reduction_rule_bis (tracker lhs) nb_cond i (no_pos P_Wild) rhs)
           in
           aux_rule i (reduc_r::acc) q
        (* [c.] If the rule has the attribut "owise". *)
        | ({elt=(lhs,rhs);_}, OwiseRule, _)::q ->
           let owise_r = no_pos (create_otherwise_rule_bis (tracker lhs) nb_cond rhs) in
           aux_rule i (owise_r::acc) q
      in
      let new_acc_sym = if List.mem flat_head_sym acc_sym then acc_sym else flat_head_sym::acc_sym in
      new_acc_sym, aux_rule 0 [encap_r] l@acc_rule
  in
  StrMap.fold aux_sigma equiv_class ([flat_inj_sym;flat_sym;flat_bool_sym], []) (* @FIX add bemolBool en symbole ! *)
            (*  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
