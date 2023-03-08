open LP.Syntax
open Common.Type

open Interface.LP_p_term
open Interface.Getter_term

let sort_to_p_symbol : sort -> p_command = fun s ->
  let sort_type = get_sort_type s in
  let res = create_p_symbol [] s [] (Some sort_type) None in
  create_LP_symbol res (* TODO modifier = Const ? *)
