// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command =
  | Print of Expression
  | Run
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State =
  {
    Program : list<int * Command>
    // TODO: Add variable context to the program state
    VariableContext : Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
  // TODO: Add support for printing NumberValue and BoolValue
  match value with
  | StringValue s -> printf "%s" s
  | BoolValue b -> printf "%b" b
  | NumberValue n -> printf "%d" n

let getLine state line =
  state.Program |> List.find (fun (l, _) -> l = line)

let addLine state (line, cmd) =
  match state.Program |> List.tryFind (fun (l, _) -> l = line) with
  | Some _ ->
      { state with Program = state.Program |> List.map (fun (l, c) -> if l = line then (l, cmd) else (l, c)) }
  | None ->
      { state with Program = (line, cmd) :: state.Program |> List.sortBy fst }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr =
  // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression'
  // in order to be able to handle variables!
  match expr with
  | Const value -> value
  | Function("-", [e1; e2]) ->
      let v1 = evalExpression state e1
      let v2 = evalExpression state e2
      match v1, v2 with
      | NumberValue n1, NumberValue n2 -> NumberValue (n1 - n2)
      | _ -> failwith "invalid arguments to '-'"
  | Function("=", [e1; e2]) ->
      let v1 = evalExpression state e1
      let v2 = evalExpression state e2
      match v1, v2 with
      | NumberValue n1, NumberValue n2 -> BoolValue (n1 = n2)
      | StringValue s1, StringValue s2 -> BoolValue (s1 = s2)
      | BoolValue b1, BoolValue b2 -> BoolValue (b1 = b2)
      | _ -> failwith "invalid arguments to '='"
  | Variable name ->
      match state.VariableContext |> Map.tryFind name with
      | Some value -> value
      | None -> failwith "variable not found"


let rec runCommand state (line, cmd) =
  match cmd with
  | Run ->
      let first = List.head state.Program
      runCommand state first
  | Print(expr) ->
      let value = evalExpression state expr
      printValue value
      runNextLine state line
  | Goto(line) ->
      let line = getLine state line
      runCommand state line

  // TODO: Implement assignment and conditional. Assignment should run the
  // next line after setting the variable value. 'If' is a bit trickier:
  // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
  // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
  // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
  //
  // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
  // the command in the 'THEN' branch and the current line as the line number.
  | Assign(s, expression) ->
      let value = evalExpression state expression
      let newVariables = state.VariableContext |> Map.add s value
      runNextLine { state with VariableContext = newVariables } line
  | If(expression, command) ->
      let value = evalExpression state expression
      match value with
      | BoolValue true -> runCommand state (line, command)
      | BoolValue false -> runNextLine state line
      | _ -> failwith "invalid arguments to 'IF'"


and runNextLine state line =
    let findResult = state.Program |> List.tryFind (fun (l, _) -> l > line)
    match findResult with
    | Some line -> runCommand state line
    | None -> state


// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
    match line with
    | Some ln -> addLine state (ln, cmd)
    | None -> runCommand state (System.Int32.MaxValue, cmd)


let runInputs state cmds =
  cmds |> List.fold runInput state

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []; VariableContext = Map.empty } // TODO: Add empty variables to the initial state!

let helloOnce =
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n"))
    Some 10, Print (Const (StringValue "HELLO NPRG077\n"))
    None, Run ]

let helloInf =
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n"))
    Some 10, Print (Const (StringValue "HELLO NPRG077\n"))
    None, Run ]

let testVariables =
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n"))
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S")
    Some 50, Print(Variable "I")
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function)
runInputs empty testVariables |> ignore

let helloTen =
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n"))
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue ""))
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
