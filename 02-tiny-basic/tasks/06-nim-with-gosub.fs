// ----------------------------------------------------------------------------
// 06 - Add support for more elegant programs with GOSUB
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
  | Print of Expression list
  | Input of string
  | Stop
  // NOTE: Add the GOSUB jump and RETURN commands
  | GoSub of int
  | Return

type State =
  { Program : list<int * Command>
    Variables : Map<string, Value>
    Random : System.Random
    // TODO: Add a stack of line numbers to return to (list<int>)
    Stack: list<int>
    }

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
  | Goto(newLine) ->
      let newlineAndCmd = getLine state newLine
      runCommand state newlineAndCmd
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

  // TODO: GOSUB needs to store the current line number on the stack for
  // RETURN (before behaving as GOTO); RETURN pops a line number from the
  // stack and runs the line after the one from the stack.
  | GoSub newLine ->
      let newState = { state with Stack = line :: state.Stack }
      let newlineAndCmd = getLine state newLine
      runCommand newState newlineAndCmd
  | Return ->
      match state.Stack with
      | [] -> failwith "RETURN without GOSUB"
      | stackLine :: rest ->
        let newState = { state with Stack = rest }
        runNextLine newState stackLine

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

// TODO: Add empty stack of return line numbers here
let empty = { Program = []; Variables = Map.empty; Random = Random(); Stack = [] }

let nim =
  [ Some 10, Assign("M", num 20)
    Some 20, Assign("U", num 1)
    Some 30, GoSub(100)
    Some 40, Assign("U", num 2)
    Some 50, GoSub(100)
    Some 60, Goto(20)
    Some 100, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 110, Print [ str "PLAYER "; var "U"; str ": YOU CAN TAKE BETWEEN 1 AND ";
      Function("MIN", [num 5; var "M"]); str " MATCHES\n" ]
    Some 120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 130, Input("P")
    Some 140, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 120)
    Some 150, Assign("M", var "M" .- var "P")
    Some 160, If(var "M" .= num 0, Goto 200)
    Some 170, Return
    Some 200, Print [str "PLAYER "; var "U"; str " WINS!"]
    None, Run
  ]

runInputs empty nim |> ignore
