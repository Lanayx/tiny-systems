// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
// ----------------------------------------------------------------------------

type Address = int * int

type Value =
  | Number of int
  | String of string
  | Error of string

type Expr =
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

type CellNode =
  { mutable Value : Value
    mutable Expr : Expr
    Updated : Event<unit> }

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr =
  match expr with
  | Const(v) -> v
  | Reference(a) ->
    match Map.tryFind a sheet with
    | Some(e) -> e.Value
    | None -> Error <| sprintf "Missing value at address %O" a
  | Function("+", [a; b]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) -> Number(x + y)
    | a, b -> Error <| sprintf "+ Invalid arguments %O and %O" a b
  | Function("-", [a; b]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) -> Number(x - y)
    | a, b -> Error <| sprintf "- Invalid arguments %O and %O" a b
  | Function("*", [a; b]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) -> Number(x * y)
    | a, b -> Error <| sprintf "* Invalid arguments %O and %O" a b
  | Function("/", [a; b]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) -> Number(x / y)
    | a, b -> Error <| sprintf "/ Invalid arguments %O and %O" a b
  | Function(name, _) -> Error $"Unknown function: {name}"

let rec collectReferences (expr:Expr) : Address list =
  match expr with
  | Const(_) -> []
  | Reference(a) -> [a]
  | Function(_, args) ->
     match args with
      | [] -> []
      | _ -> List.collect collectReferences args

let makeNode (sheet:LiveSheet) expr =
  match expr with
  | Const(v) ->
    { Value = v; Expr = expr; Updated = Event<unit>() }
  | Reference(a) ->
    match Map.tryFind a sheet with
    | Some(e) ->
      let result = { Value = e.Value; Expr = expr; Updated = Event<unit>() }
      result.Updated.Publish.AddHandler(fun _ () ->
        result.Value <- e.Value
        result.Updated.Trigger()
      )
      result
    | None -> failwith $"Missing value at address {a}"
  | Function(_, args) ->
    let node = { Value = eval sheet expr; Expr = expr; Updated = Event<unit>() }
    let update _ () =
      let newValue = eval sheet expr
      node.Value <- newValue
      node.Updated.Trigger()
    for parentCellAddress in List.collect collectReferences args do
      match Map.tryFind parentCellAddress sheet with
      | Some(e) -> e.Updated.Publish.AddHandler(update)
      | None -> failwith $"Missing value at address {parentCellAddress}"
    node

let updateNode addr (sheet:LiveSheet) expr =
  match Map.tryFind addr sheet with
  | Some(node) ->
    node.Expr <- expr
    node.Value <- eval sheet expr
    node.Updated.Trigger()
  | None -> failwith $"Missing value at address {addr}"

let makeSheet list =
  let addNode sheet (addr, expr) =
    Map.add addr (makeNode sheet expr) sheet
  list |> List.fold addNode Map.empty

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) =
  match srcExpr with
  | Const v -> Const v
  | Reference (col, row) -> Reference (col + tgtCol - srcCol, row + tgtRow - srcRow)
  | Function (name, args) -> Function (name, List.map (relocateReferences (srcCol, srcRow) (tgtCol, tgtRow)) args)

let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:LiveSheet) : LiveSheet =
  [ for i in srcRow..tgtRow do
      for j in srcCol..tgtCol do
        let refCell = Map.find (srcCol, srcRow) sheet
        let newReferences = relocateReferences (srcCol, srcRow) (j, i) refCell.Expr
        yield (j, i), newReferences ]
  |> List.fold (fun s (k, v) -> Map.add k (makeNode s v) s) sheet

// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  // TODO: Turn the given value into a string representing HTML
  // You can use the following to create an error string in red.
  //"<span class='e'>not implemented</span>"

  match v with
  | Number n -> $"<i>{n}</i>"
  | String s -> $"<b>{s}</b>"
  | Error e -> $"<span class='e'>{e}</span>"

let addr (s:string) =
  let col = int s[0] - int 'A' + 1
  let row = int s[1..]
  col, row

let getColLetter (col:int) =
  let letter = char (int 'A' + col - 1)
  letter.ToString()

let display (sheet:LiveSheet) =
  // TODO: Find the greatest row and column index
  let maxCol = sheet |> Map.fold (fun acc (col, _) _ -> max acc col) 0
  let maxRow = sheet |> Map.fold (fun acc (_, row) _ -> max acc row) 0

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; }
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; }
        th { background:#606060; color:white; }
      </style>
    </head><body><table>""")

  // TODO: Write column headings
  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do
    wr.Write($"<th>{getColLetter col}</th>")
  wr.Write("</tr>")

  // TODO: Write row headings and data
  for row in 1 .. maxRow do
    wr.Write($"<tr><th>{row}</th>")
    for col in 1 .. maxCol do
      match Map.tryFind (col, row) sheet with
      | None -> wr.Write("<td></td>")
      | Some(cell) -> wr.Write($"<td>{displayValue cell.Value}</td>")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------



// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =
  [ addr "A1", Const(Number 0)
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")
display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac =
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")
display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4!
let tempConv =
  [ addr "A1", Const(String "F to C")
    addr "B1", Const(Number 0)
    addr "C1",
      Function("/", [
        Function("*", [
          Function("-", [ Reference(addr "B1"); Const(Number 32) ])
          Const(Number 5) ])
        Const(Number 9) ])
    addr "A2", Const(String "C to F")
    addr "B2", Const(Number 0)
    addr "C2",
      Function("+", [
        Function("/", [
          Function("*", [ Reference(addr "B2"); Const(Number 9) ])
          Const(Number 5) ])
        Const(Number 32) ])]
  |> makeSheet
display tempConv
