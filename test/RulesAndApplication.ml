open Alcotest
open TestInfra

let bigraph_1 = Some( Eval.parseBG ("Bigraphfiles/Security/comms.json") ) 

let test_option () = 
  let result =  Some (Eval.parseBG ("Bigraphfiles/Security/comms.json")) in
  let expected = (bigraph_1) in 
  check testable_bg "option success" (result)  (expected) 

let test_rule_app ruleID () =
  let result = Eval.execute_rule (parse_test_rules ruleID) (bigraph_1) in
  let expected = (bigraph_1) in
  check testable_bg "Rule applied successfully" (result) (expected)

let test_rule_fail ruleID () =
  let result = Eval.execute_rule (parse_test_rules ruleID) (bigraph_1) in
  let expected = (None) in
  check testable_bg "Rule Failed successfully" (result) (expected)

(** This isn't quite strict enough since we need to have a seperate function for distinguishing
rule types but functionallty this works anyway since the test rules dont have conditions*)
let test_condRule_fail ruleID () = 
  let result = Eval.execute_cond_rule (parse_test_rules ruleID) bigraph_1 in
  let expected = (None) in
  check testable_bg "Cond Rule Failed Successfully" (result) (expected)

let suite =
  [ 
    "can parse Bg", `Quick, test_option;
    "can apply place only rule", `Quick, test_rule_app "Bigraphfiles/Security/child_id.json" ;
    "can apply link only rule", `Quick, test_rule_app "Bigraphfiles/Security/camera_id.json" ;
    "can Fail rule", `Quick, test_rule_fail "Bigraphfiles/Security/child_fail.json";
    "can Fail cond rule", `Quick, test_condRule_fail "Bigraphfiles/Security/cond_fail.json";
  ]

let () =
  Alcotest.run "Basic suite Execution" [ "Basics", suite ]