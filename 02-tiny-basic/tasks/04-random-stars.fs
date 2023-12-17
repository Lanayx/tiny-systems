// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command =
  | Print of Expression
  | Run
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State =
  { Program : list<int * Command>
    Variables : Map<string, Value>
    // TODO: You will need to include random number generator in the state!
    Random: Random }

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

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args =
  match args with
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
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
      match state.Variables |> Map.tryFind name with
      | Some value -> value
      | None -> failwith "variable not found"
  | Function ("||", [e1; e2]) ->
      let v1 = evalExpression state e1
      let v2 = evalExpression state e2
      match v1, v2 with
      | BoolValue b1, BoolValue b2 -> BoolValue (b1 || b2)
      | _ -> failwith "invalid arguments to '||'"
  | Function ("<", [e1; e2]) ->
      let v1 = evalExpression state e1
      let v2 = evalExpression state e2
      binaryRelOp (<) [v1; v2]
  | Function (">", [e1; e2]) ->
      let v1 = evalExpression state e1
      let v2 = evalExpression state e2
      binaryRelOp (>) [v1; v2]
  | Function ("RND", [e]) ->
      let n = evalExpression state e
      match n with
      | NumberValue n -> NumberValue (state.Random.Next(0, n))
      | _ -> failwith "invalid arguments to 'RND'"

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
  | Assign(s, expression) ->
      let value = evalExpression state expression
      let newVariables = state.Variables |> Map.add s value
      runNextLine { state with Variables = newVariables } line
  | If(expression, command) ->
      let value = evalExpression state expression
      match value with
      | BoolValue true -> runCommand state (line, command)
      | BoolValue false -> runNextLine state line
      | _ -> failwith "invalid arguments to 'IF'"

  // TODO: Implement two commands for screen manipulation
  | Clear ->
      Console.Clear()
      runNextLine state line
  | Poke (ex, ey, evalue) ->
      let x = evalExpression state ex
      let y = evalExpression state ey
      let value = evalExpression state evalue
      match x, y, value with
      | NumberValue x, NumberValue y, StringValue value ->
        Console.SetCursorPosition(x, y)
        printf "%s" value
        runNextLine state line
      | _ ->
        failwith "invalid arguments to POKE"


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
    | None -> runCommand state (Int32.MaxValue, cmd)
let runInputs state cmds =
  cmds |> List.fold runInput state

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.:
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars =
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num Console.WindowWidth], "RND" @ [num Console.WindowHeight], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num Console.WindowWidth], "RND" @ [num Console.WindowHeight], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40))
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
Console.CursorVisible <- false
runInputs empty stars |> ignore
