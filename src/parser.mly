%{
    open Ast    
%}

%token <string> ID
%token LPAREN
%token SEMICOLON
%token DOUBLESEMICOLON
%token REPEAT
%token RPAREN
%token LET
%token APPLY
%token STRAT
%token CONDLET
%token EQUALS
%token BEQUALS
%token EOF
%token IF
%token THEN
%token LBRACE
%token RBRACE
%token WHILE
%token CHOICE
%token ELSE
%token ANY
%token <int> INT
%token APPCOND
%token USING
%token IN
%token PARAM
%token CTX
%token BANG
%token COMMA
%token UPTO
%token USE  
%token LSQUARE
%token IDENTITY
%token CHECK
%token RSQUARE
%token FAIL

%start file
%type <Ast.toplevel_cmd list> file

%left SEMICOLON CHOICE 

%%

file:
  | EOF                      { [] }
  | command DOUBLESEMICOLON file  { $1 :: $3 }
  | command EOF              { [$1] }

command:
  | definition           { Def $1 }
  | STRAT ID BEQUALS LBRACE strategy { Strategy($2,$5)}
  | APPLY; e1 = ID  { Application(e1) }

strategy:
  | RBRACE { Identity }
  | strategy_expr RBRACE { $1 }

strategy_expr:
  | s=ID { CallStrategy(s) }
  | IDENTITY { Identity }
  | FAIL { Fail }
  | CHECK; LPAREN; b=ID; RPAREN; { Check(b) }
  | IF; LPAREN; s1=strategy_expr; RPAREN; THEN; LPAREN; s2=strategy_expr; RPAREN; ELSE; LPAREN; s3=strategy_expr; RPAREN {Conditional(s1,s2,s3)}
  | WHILE; LPAREN; s1=strategy_expr; RPAREN;  {CondLoop(s1)}
  | s=separated_pair(strategy_expr, CHOICE, strategy_expr) { Choice(s) }
  | s=separated_pair(strategy_expr, SEMICOLON, strategy_expr) { Sequence(s)}
  | ANY; LPAREN; ss=separated_nonempty_list(COMMA, strategy_expr); RPAREN; { Any(ss) }
  | REPEAT; LPAREN; i = INT; RPAREN; USE ;LPAREN; s1=strategy_expr; RPAREN;  { ForLoop(i,s1)  }
  | UPTO; LPAREN; i = INT; RPAREN; USE ;LPAREN; s1=strategy_expr; RPAREN;  { UptoLoop(i,s1)  }
  | LPAREN; s=strategy_expr; RPAREN; { s }   

cond:
  LPAREN option(is_bang) IN place USING LSQUARE ID RSQUARE RPAREN { CondExp($2,$4,$7) };

is_bang:
  | BANG { true }

place:
  | PARAM { Cond_Param }
  | CTX   { Cond_Ctx } 

definition:
  | LET; e1 = ID; EQUALS; LSQUARE; e2 = ID; RSQUARE { Ruledef(e1,e2) }
  | CONDLET; e1=ID; EQUALS; LSQUARE; e2=ID; RSQUARE; APPCOND; LPAREN; a1=separated_nonempty_list(COMMA, cond); RPAREN;  { CondRuledef(e1,e2,a1) }