# Manual

The Heist interpreter executes programs written in the Heist programming language stored in *.Heat* files, commands in each file are executed sequentially and are delimeted by ```;;```.

## Heist Language Syntax 

### Reaction rules and conditional reaction rules

Reaction rules and conditional reaction rules are imported from the **JSON** format specificied by BigraphER and perform fundamental rewriting steps when invoked using a ```do``` statement.

```
let rule = [path_to_rule];;

let cond_rule = [path_to_rule]appcond( (! in + using [path_to_bg]), ... );;

```

### Strategy expressions

Strategy expressions give context and control to how rules are applied all may be invoked independently using a ```do``` statement after defintion. Strategy expression arguments are themselves strategy expressions which eventually reduce to rules.

#### Identity and Fail

```
do ID;;
do Fail;;
```

#### Conditional strategy

```
do if(strategy_expr)then(strategy_expr)else(strategy_expr);;
```

#### Sequencing

Fixed length sequences use a semicolon to assert that all substrategies should apply in order for a sequence to be successfull.

```
do strategy_expr ; strategy_expr ;;
```

The choice operator ? is most commonly used to define partial sequences where either the second strategy argument is only executed if the first fails.

```
do strategy_expr ? strategy_expr ;;
```
#### non-deterministic strategy

Executes one applicable strategy from a collection of possible options.

```
do Any(strateg_expr, ... , strategy_expr);;
```

#### Iteration

Heist implements three iterative strategies, conditional iteration with ```while``` and unconditional iteration defined with either ```;``` or ```?``` denoted by ```for``` and ```upto``` respectivley.

```
do while( strategy_expr );;
do for( int )use( strategy_expr );;
do upto( int )use( strategy_expr );;
```

#### Checking strategy

The ```Check``` strategy can be used to determine whether an arbitrary bigraph **P** exists elsewhere in the system without performing any state mutation. As with reaction rule definitions the check strategy operates using a path to a **JSON** file defining a bigraph.

```
do Check( path_to_P );;
```

### User defined strategies

Users can define their own named strategies with the ```strat``` keyword which as with standalone strategies can be executed sequentailly using ```do```.

```
strat example_strategy := { strategy_expr } ;;

do example_strategy;;
```

## Executing Heat programs

Once a valid program has been created in a *.heat* file execution can be performed with the interpreter *heist.exe* using the command line.

```
$ ./heist.exe path_to_heat_file path_to_initial_bg
```

#### Optional arguments

| Argument         | description     | 
|--------------|-----------|
| ```-t filepath``` | Write final bigraph state to **tikz**       |
| ```-t json```      | Write final bigraph state to **JSON**  | 

Sample programs are provided in the ```test/Heat``` directory with equivalent *BigraphER* representations, prewritten commands to execute these sample programs are given in ```test/Macros.txt```.  
