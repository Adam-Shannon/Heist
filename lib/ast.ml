type cond_where = Cond_Ctx | Cond_Param

type cond_exp = 
  | CondExp of (bool option)*cond_where*string 

 type def =
  | Ruledef of string * string
  | CondRuledef of string * string * (cond_exp list)

type strategy_expr =
  | CallStrategy of string
  | Conditional of strategy_expr*strategy_expr*strategy_expr
  | CondLoop of strategy_expr
  | Choice of (strategy_expr*strategy_expr)
  | Sequence of (strategy_expr*strategy_expr)
  | Any of strategy_expr list
  | ForLoop of int*strategy_expr
  | UptoLoop of int*strategy_expr
  | Check of string
  | Identity
  | Fail 

type toplevel_cmd =
  | Def of def (*parsing reaction rules *)
  | Strategy of string*(strategy_expr)
  | Application of string