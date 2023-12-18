// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  | Run
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string
  | Stop

type State =
  { Program : list<int * Command>
    Variables : Map<string, Value>
    Random : Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
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

let binaryRelOp f args =
  match args with
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
  // TODO: We need an extra function 'MIN' that returns the smaller of
  // the two given numbers (in F#, the function 'min' does exactly this.)
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
  | Function ("MIN", [e1; e2]) ->
      let v1 = evalExpression state e1
      let v2 = evalExpression state e2
      match v1, v2 with
      | NumberValue n1, NumberValue n2 -> NumberValue (min n1 n2)
      | _ -> failwith "invalid arguments to 'MIN'"

let rec runCommand state (line, cmd) =
  match cmd with
  | Run ->
      let first = List.head state.Program
      runCommand state first

  | Print(exprs) ->
      for expr in exprs do
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

  // TODO: Input("X") should read a number from the console using Console.RadLine
  // and parse it as a number using Int32.TryParse (retry if the input is wrong)
  // Stop terminates the execution (you can just return the 'state'.)
  | Input s ->
      let rec readInput() =
          let input = Console.ReadLine()
          let value = Int32.TryParse(input)
          match value with
          | true, value -> value
          | _ -> readInput()
      let value = readInput()
      let newVariables = state.Variables |> Map.add s (NumberValue value)
      runNextLine { state with Variables = newVariables } line
  | Stop ->
      state

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim =
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND ";
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND ";
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
