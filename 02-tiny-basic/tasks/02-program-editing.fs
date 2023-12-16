// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression =
  | Const of Value

type Command =
  | Print of Expression
  | Run
  | Goto of int

type State =
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
  match value with
  | StringValue s -> printf "%s" s

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

let rec evalExpression expr =
  match expr with
  | Const value -> value

let rec runCommand state (line, cmd) =
  match cmd with
  | Run ->
      let first = List.head state.Program
      runCommand state first
  | Print(expr) ->
      let value = evalExpression expr
      printValue value
      runNextLine state line
  | Goto(line) ->
      let line = getLine state line
      runCommand state line

and runNextLine state line =
    let findResult = state.Program |> List.tryFind (fun (l, _) -> l > line)
    match findResult with
    | Some line -> runCommand state line
    | None -> state


// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  // TODO: Simulate what happens when the user enters a line of code in the
  // interactive terminal. If the 'line' number is 'Some ln', we want to
  // insert the line into the right location of the program (addLine); if it
  // is 'None', then we want to run it immediately. To make sure that
  // 'runCommand' does not try to run anything afterwards, you can pass
  // 'System.Int32.MaxValue' as the line number to it (or you could use -1
  // and handle that case specially in 'runNextLine')
    match line with
    | Some ln -> addLine state (ln, cmd)
    | None -> runCommand state (System.Int32.MaxValue, cmd)


let runInputs state cmds =
  // TODO: Apply all the specified commands to the program state using 'runInput'.
  // This is a one-liner if you use 'List.fold' which has the following type:
  //   ('State -> 'T -> 'State) -> 'State -> list<'T> -> 'State
  cmds |> List.fold runInput state

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce =
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n"))
    Some 10, Print (Const (StringValue "HELLO NPRG077\n"))
    None, Run ]

let helloInf =
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n"))
    Some 10, Print (Const (StringValue "HELLO NPRG077\n"))
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
