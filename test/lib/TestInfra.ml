open Bigraph
open Eval

module S = Solver.Make_SAT (Solver.MS)

let bg_pp ppf =  function
| Some b' -> Big.pp ppf b'
| None -> Fmt.pf ppf "No bigraph" 

(** equality check is more like check if something has chnaged to anything else due to eval formats  *)
let bg_eq (b:Eval.state_bigraph) (b1:Eval.state_bigraph) = match b,b1 with
  | Some b_r, Some b_e -> S.equal b_r b_e
  | None , None -> true
  | _ -> false

let testable_bg = Alcotest.testable (bg_pp) (bg_eq)

let parse_test_rules path = Eval.rule_from_json path  

(**Unsafe Assumes path ends in / *)
let create_rule_strat path filename = 
  let rname = List.hd (String.split_on_char '.' filename) in
    let rule_in = Eval.rule_from_json (String.cat path filename) in
      (rname, Rule rule_in) 

(** Unsafe - attempts to pass all files in Dir as rules => testfiles/rules/*.json should be used *)
let rules_from_dir path = Sys.readdir path |> Array.to_list |> List.map( create_rule_strat path )


