open Alcotest
open Eval
open TestInfra

(** Testing Bigraph imports and environment building *)
let a_state = Some( Eval.parseBG ("Bigraphfiles/tester/a_state.json") )
let b_state = Some( Eval.parseBG ("Bigraphfiles/tester/b_state.json") ) 
let c_state = Some( Eval.parseBG ("Bigraphfiles/tester/c_state.json") ) 


let build_lookup_table = rules_from_dir "Bigraphfiles/tester/rules/" 


(** Simple sequence strategy A --> C ; C --> A *)
let build_simple_sequence = Strat (
  Sequence(
  CallStrategy "a_to_c",
  CallStrategy "c_to_a" 
  )
)

(** Simple conditional strategy  if(C-->A)then(A-->C)else(A-->B) *)

let build_cond_sequence = Strat ( Conditional (
  CallStrategy "c_to_a" ,
  CallStrategy "a_to_c" ,
  CallStrategy "a_to_b" 
  )
  )

(** Test conditional iterator while(A-->C) takes A => C (single iter) *)
let build_while_sequence = Strat ( CondLoop (
  CallStrategy "a_to_c" 
  )
  )

(** Test choice application - (C --> A) > ( A --> B) *)
let build_choice_sequence = Strat ( Choice (
  CallStrategy "c_to_a" ,
  CallStrategy "a_to_b" 
  )
  )

(** Test For loop application for(3)( A->C ); A-> Fail *)
let build_for_sequence = Strat ( ForLoop (3,
  CallStrategy "a_to_c" 
  )
  )

(** Test upto Loop application for(3)( A->C ); A -> C *)
let build_upto_sequence = Strat ( 
  UptoLoop (3,
  CallStrategy "a_to_c" 
  )
  )

(** Test Check application  A->C;Check(C); -- <ID,C> *)
let build_check_sequence = Strat (
  Sequence (
  CallStrategy "a_to_c" ,
  Check "Bigraphfiles/tester/c_state.json"
  )
  )

(** Test Idenity sequence (A->A)*) 
let build_id_sequence = Strat (
  Identity
  )   

(** Test Fail sequence (A->Fail)*)  
let build_fail_sequence = Strat (
  Fail
)

(** Strategy test driver *)
let test_strategy strategy initial final () = 
  let result = snd( Eval.eval_strategy (strategy) (build_lookup_table) (initial)) in
  let expected = (final) in
  check testable_bg "While loop singular execution" (result) (expected)

(** Test suite definition *)
let suite =
  [ 
    "Simple sequence", `Quick, test_strategy (build_simple_sequence) (a_state) (a_state);
    "Conditional then branch", `Quick, test_strategy (build_cond_sequence) (c_state) (c_state);
    "Conditional else branch", `Quick, test_strategy (build_cond_sequence) (a_state) (b_state);
    "While iterator", `Quick, test_strategy (build_while_sequence) (a_state) (c_state);
    "Choice", `Quick, test_strategy (build_choice_sequence) (a_state) (b_state); 
    "For iterator",  `Quick, test_strategy (build_for_sequence) (a_state) (None);
    "Upto iterator",  `Quick, test_strategy (build_upto_sequence) (a_state) (c_state);
    "Check sequence", `Quick, test_strategy (build_check_sequence) (a_state) (c_state);
    "Identity execution", `Quick, test_strategy (build_id_sequence) (a_state) (a_state);
    "Fail execution", `Quick, test_strategy (build_fail_sequence) (a_state) (None);
  ]

let () =
  Alcotest.run "Strategy evaluation suite" [ "Successfull Strategy Evaluation", suite ]