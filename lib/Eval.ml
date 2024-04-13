open Bigraph
open Ast

(** MiniSAT Solver module *)
module S = Solver.Make_SAT (Solver.MS)
(** BRS module *)
module Execute = Brs.Make(S)
(** Conditional Reaction rule module *)
module CondReactChecker = AppCond.Make(S)

(** Fundamental type of a strategy expression derived from the AST *)
type strategy_def = (Ast.strategy_expr);;

(** Applicable type separating mutating actions from strategies *)
type applicable =  Rule of Execute.react | CondRule of Execute.react | Strat of strategy_def | None;;

(** Definition type for BG and Rules w/ algebra *)
type definition = RDef of string 

(** type of the global environment lookup table*)
type global = Def of definition | App of applicable
type global_strats = (string * global) list

(** type of the Bigraph being acted on by strategies - Some bigraph.t 
indicates a successfull application and None is failure *)
type state_bigraph = Big.t option

(** type of the execution environment for a Heist program *)
type state =  (global_strats * state_bigraph) 

(** File parser for .heat files used *)
let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s;;

(** Function to parse bigraphs from JSON *)
let parseBG filename =
  let json_string = read_whole_file filename in
    let json = Yojson.Safe.from_string json_string in 
      Big.t_of_yojson json

(** Function to parse reaction rules from JSON *)
let rule_from_json filename = 
  let json_string = read_whole_file filename in
    let json = Yojson.Safe.from_string json_string in
      Execute.react_of_yojson json
      

(** Specialised Parsing method for a series of application conditions independently from a rule *)
let rec parseConds (conds:(cond_exp list)) = 
  match conds with
    | [] -> []
    | c::cs -> match c with | CondExp (a,b,c) -> ({neg=(get_cond a); where=(get_loc b); pred=(parseBG c)}:AppCond.t) :: parseConds cs 

and get_cond (bang:(bool option)) = match bang with | Some bool -> bool | None -> false
(** Additional parsing mechanisms  for the optional negation in an application condition 
and the location of the match*)
and get_loc  (loc:(Ast.cond_where)) = match loc with | Cond_Ctx -> (Ctx) | Cond_Param -> (Param) 

(** Parse reaction rule (from the a strategy program rather than JSON)
including conditionals *)     
let parseRule (def) = match def with 
  | Ruledef (n,p) -> (n, Rule(rule_from_json p))
  | CondRuledef (n,p,conds_in) -> 
    let r = rule_from_json p in
      (n, (Rule{r with conds=(parseConds conds_in)}))

(** Main parser driver function delimiting definitions and applications (via do). Applications
are ignored and only defintions and strategies are used to build the global lookup table  *)
let parse( (strats, b): state) = function
  | Ast.Def a -> 
    let (id, new_rule) = parseRule a in 
      let updated_applic_list = (id, App new_rule)::(strats) in 
        (updated_applic_list, b );
  | Ast.Strategy (id,body) ->(
      try
        let _ = List.assoc id strats in
          failwith "Strategy already exists"
      with Not_found -> ((id,App (Strat body))::strats, b))
  | Ast.Application id ->
  try 
      let _ = List.assoc id strats in  
        (strats, b);
    with Not_found -> failwith "Applied strategy doesnt exist";
  ;;


(** Strips the application conditions from a reaction rule returning a pure rule *)
let remove_conds (r:Execute.react) = {r with conds=[]}

(** execute a given reaction rule r on a state b *)
let execute_rule (r:Execute.react) (b:state_bigraph)  =  match b with 
  | Some b_ok -> let r_no_conds = remove_conds r in print_string "executing reaction rule\n"; Execute.apply b_ok [r_no_conds] 
  | None -> None   

(** execute a conditional rule r on a state b - has to be separated
due to type definition of conditional rules in bigraphER*)
let execute_cond_rule (r:Execute.react) (b:state_bigraph)  =  match b with
  | Some b_ok -> print_string "applying conditional rule instance \n";Execute.apply b_ok [r] 
  | None -> None   

(** filter - get Apps for eval env*)
let rec filter_apps (g:global_strats) =
  match g with
    | [] -> []
    | x::xs -> 
      match x with
        | (id,App a) -> (id,a)::(filter_apps xs)
        | (_,Def _) -> filter_apps xs

(** lookup a strategy with a name in the global list of type
global_strats *)
let lookup id (globs:global_strats) =
    let to_apply = List.assoc_opt id (filter_apps globs) in
      match to_apply with
        | Some body -> body
        | None -> failwith "Strategy doenst exist"

(** Using a list of strategy results - reduce to a list of only results
which are of type Some big.t *)
let rec filter_valid l =
  match l with
    | [] -> []
    | re::res -> 
      match re with
        | (_, Some _) -> re::(filter_valid res)
        | (_, None) -> filter_valid res

(** pick a random list element *)
let pick_random l =
    let max = Random.int (List.length l) in
    List.nth l max

(** driver function used to separate strategies into explicit behaviours *)
let rec step (s: strategy_expr) (globs :global_strats) (b0: Big.t) = 
  match s with
  (*Call to a rule or generalised strategy*)
  | CallStrategy id -> let body = lookup id globs in eval_strategy body globs (Some b0) 
  | Conditional (s1,s2,s3) -> step_conditional (s1,s2,s3) globs (Some b0)
  | CondLoop (s1) -> step_while(s1) globs (Some b0)
  | Choice (s1,s2) -> step_choice(s1,s2) globs (Some b0)
  | Sequence (s1,s2) -> step_sequence(s1,s2) globs (Some b0)
  | Any (ss) -> step_any (ss) globs (Some b0)
  | ForLoop (n,s1) -> step_for(n, s1) globs (Some b0)
  | UptoLoop (n,s1) -> step_upto(n, s1) globs (Some b0)
  | Check b -> step_check(b) globs (Some b0)
  | Identity -> eval_strategy (None) globs (Some b0)
  | Fail -> eval_strategy (None) globs (None)

(** Evaluate a sequence S1;S2 with leftmost precedence*)
and step_sequence (s1,s2) globs b0 =
    let (_,res_1) = eval_strategy (Strat s1) globs b0 in 
    match res_1 with
      | Some b' -> eval_strategy (Strat s2) globs (Some b')
      | None -> eval_strategy (None) globs (b0)

(** Evaluate a conditional if(S1)then(S2)else(S3) *)
and step_conditional (s1,s2,s3) globs b0 =
  let (_,res_1) = eval_strategy (Strat s1) globs b0 in 
    match res_1 with
      | Some b' -> eval_strategy (Strat s2) globs (Some b')
      | None -> eval_strategy (Strat s3) globs (b0)

(** Evaluate a while strategy while(S1) *)
and step_while (s1) globs b0 =
  let (_,res_1) = eval_strategy (Strat s1) globs b0 in 
    match res_1 with
      | None -> eval_strategy (None) globs (b0)
      | Some b' -> step_while (s1) globs (Some b')

(** Evaluate a choice S1 ? S2 with leftmost precedence *)
and step_choice (s1,s2) globs b0 =
    let (_,res_1) = eval_strategy (Strat s1) globs b0 in 
    match res_1 with
      | None -> eval_strategy (Strat s2) globs b0
      | Some b' -> eval_strategy (None) globs (Some b')

(** Evaluate an random choice strategy any([S1, ..., Sn]) *)
and step_any (ss) globs b0 = 
    let ind_results = List.map (fun s ->  print_string "evaluating within any\n";eval_strategy (Strat s) globs b0 ) (ss) in
      let valids = filter_valid ind_results in 
      match valids with
        | [] -> eval_strategy (None) globs (None)
        | _ -> eval_strategy (None) globs (snd (pick_random valids))
          
(** Evaluate a for strategy for(n)use(S1) *)          
and step_for (n,s) globs b0 =
    match n with
      | 0 -> eval_strategy (Strat s) globs b0
      | x ->  let (_,b') = eval_strategy (Strat s) globs b0 in
        step_for (x-1,s) globs b' 

(** Evaluate an upto strategy upto(n)use(S1) *)      
and step_upto (n,s) globs b0 =
    match n with
      | 0 -> eval_strategy (Strat (Choice (s, Identity))) globs b0
      | x ->  let (_,b') = eval_strategy (Strat( Choice (s,Identity))) globs b0 in
        step_upto (x-1,s) globs b' 
      
(** Evaluate a check strategy Check(P) for bigraph P in JSON*)
and step_check b globs b0 =
    let imported = parseBG b in
      let id_rule = ({name="arb_id"; rdx=(imported); rct=(imported); eta=None; conds=[]}:Brs.react) in
      let res = execute_rule id_rule b0 in 
      match res with
        | Some _ -> eval_strategy (None) globs (b0)
        | None -> eval_strategy (None) globs (None)

(** General strategy evaluator separating mutators from strategy combinators *)
and eval_strategy (s: applicable) (globs:global_strats) (b:state_bigraph) = 
  match s with
    | Rule r -> let result = execute_rule r b in (s,result)
    | CondRule r -> let result = execute_cond_rule r b in (s,result)
    | None -> (None, b)
    | Strat st -> match b with
          | None -> (None, None)
          | Some b0 -> step st globs b0 

(** Overarching evaluation function for second interpreter pass - triggers sequential
 application of each strategy preceeded by do in a program *)
let evaluate( (strats, b): state) = function
  | Ast.Def _ ->
      print_string "parse a rule import \n";
      (strats, b );
  | Ast.Strategy _ ->
      print_string "parse a strategy definition \n";
      (strats, b);
  | Ast.Application id ->
      print_string "evaluate a strategy application (top-level) \n";
        let lookup = List.assoc_opt id (filter_apps strats) in 
          match lookup with
            | None -> failwith "Strategy called doesn't exist"
            | Some strat_todo -> 
                print_string "lookup good \n"; 
                let (_,b') = (eval_strategy strat_todo strats b) in 
                  (strats,b');
        