// ----------------------------------------------------------------------------
// 08 - Adding change visualization
// ----------------------------------------------------------------------------

open System
open System.Text.RegularExpressions

type Location = Fixed of int | Normal of int
type RawAddress = int * int
type Address = Location * Location

let (|Raw|) (l: Address) : RawAddress =
  match l with
  | Fixed(c), Fixed(r)
  | Fixed(c), Normal(r)
  | Normal(c), Fixed(r)
  | Normal(c), Normal(r) -> (c, r)

type Value =
  | Number of int
  | String of string
  | Error of string
  | Array of Value list

type Expr =
  | Const of Value
  | Reference of Address
  | Function of string * Expr list
  | Range of Address * Address

type CellNode =
  { mutable Value : Value
    mutable Expr : Expr
    // NOTE: Set to 'true' when the cell is created or modified.
    // Used to highlight changes. Touched can later be cleared.
    mutable Touched : bool
    Updated : Event<unit> }

type LiveSheet = Map<RawAddress, CellNode>

let clean (sheet:LiveSheet) =
  // TODO: Implement a helper that sets Touched <- false
  // for all cell nodes in a given sheet.
  for cell in sheet.Values do
    cell.Touched <- false

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

module List =
  let tryMapAll f list =
    let rec loop acc = function
      | [] -> Some(List.rev acc)
      | x::xs ->
        match f x with
        | Some y -> loop (y::acc) xs
        | None -> None
    loop [] list

let rec eval (sheet:LiveSheet) expr =
  match expr with
  | Const(v) -> v
  | Reference(Raw a) ->
    match Map.tryFind a sheet with
    | Some(e) -> e.Value
    | None -> Error <| sprintf "Missing value at address %O" a
  | Range(Raw (lcol, lrow), Raw (rcol, rrow)) ->
    [for c in lcol..rcol do
      for r in lrow..rrow do
        (c, r)]
    |> List.tryMapAll (fun a -> Map.tryFind a sheet)
    |> Option.map (fun l -> Array (List.map _.Value l))
    |> Option.defaultValue (Error "Missing value in range")
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
  | Function("SUM", [Range(a, b)]) ->
    match eval sheet (Range(a, b)) with
    | Array(l) -> List.tryMapAll (fun v -> match v with | Number(x) -> Some(x) | _ -> None) l
                   |> Option.map (fun l -> Number(List.sum l))
                   |> Option.defaultValue (Error "SUM: Invalid arguments")
    | _ -> Error "SUM: Invalid arguments"
  | Function(name, _) -> Error $"Unknown function: {name}"

let rec collectReferences expr =
  match expr with
  | Const(_) -> []
  | Reference(Raw a) -> [a]
  | Range(Raw a, Raw b) -> [a; b]
  | Function(_, args) ->
     match args with
      | [] -> []
      | _ -> List.collect collectReferences args


let makeNode (sheet:LiveSheet) expr =
  match expr with
  | Const(v) ->
    { Value = v; Expr = expr; Touched = false; Updated = Event<unit>() }
  | Reference(Raw a) ->
    match Map.tryFind a sheet with
    | Some(e) ->
      let result = { Value = e.Value; Expr = expr; Touched = false; Updated = Event<unit>() }
      e.Updated.Publish.AddHandler(fun _ () ->
        result.Value <- e.Value
        result.Touched <- true
        result.Updated.Trigger()
      )
      result
    | None -> failwith $"Missing value at address {a}"
  | Range(Raw a, Raw b) ->
    match List.tryMapAll (fun a -> Map.tryFind a sheet) [a; b] with
    | Some([e1; e2]) ->
      let result = { Value = Array([e1.Value; e2.Value]); Expr = expr; Touched = false; Updated = Event<unit>() }
      let handler _ () =
        result.Value <- Array([e1.Value; e2.Value])
        result.Touched <- true
        result.Updated.Trigger()
      e1.Updated.Publish.AddHandler(handler)
      e2.Updated.Publish.AddHandler(handler)
      result
    | _ -> failwith "Missing value in range"
  | Function(_, args) ->
    let node = { Value = eval sheet expr; Expr = expr; Touched = false; Updated = Event<unit>() }
    let update _ () =
      let newValue = eval sheet expr
      node.Value <- newValue
      node.Touched <- true
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
    node.Touched <- true
    node.Updated.Trigger()
  | None -> failwith $"Missing value at address {addr}"

let makeSheet list =
  let addNode sheet (addr, expr) =
    Map.add addr (makeNode sheet expr) sheet
  list |> List.fold addNode Map.empty

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let relocateLocation (loc:Location) (by:int) : Location =
  match loc with
  | Fixed _ -> loc
  | Normal n -> Normal (n + by)

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) =
  match srcExpr with
  | Const v -> Const v
  | Reference (col, row) -> Reference (relocateLocation col (tgtCol - srcCol), relocateLocation row (tgtRow - srcRow))
  | Range ((cola, rowa), (colb, rowb)) ->
    Range ((relocateLocation cola (tgtCol - srcCol), relocateLocation rowa (tgtRow - srcRow)),
           (relocateLocation colb (tgtCol - srcCol), relocateLocation rowb (tgtRow - srcRow)))
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
  match v with
  | Number n -> $"<i>{n}</i>"
  | String s -> $"<b>{s}</b>"
  | Error e -> $"<span class='e'>{e}</span>"
  | Array l -> "<span class='e'>Array can't be displayed</span>"

let getColLetter (col:int) =
  let letter = char (int 'A' + col - 1)
  letter.ToString()

let display (sheet:LiveSheet) =
  // TODO: Modify the rendering to add green background for
  // cells that have been modified recently (Touched = true)
  let maxCol = sheet |> Map.fold (fun acc (col, _) _ -> max acc col) 0
  let maxRow = sheet |> Map.fold (fun acc (_, row) _ -> max acc row) 0

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; }
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } .touched { background:#60A060; }
        th { background:#606060; color:white; }
      </style>
    </head><body><table>""")

  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do
    wr.Write($"<th>{getColLetter col}</th>")
  wr.Write("</tr>")

  for row in 1 .. maxRow do
    wr.Write($"<tr><th>{row}</th>")
    for col in 1 .. maxCol do
      match Map.tryFind (col, row) sheet with
      | None -> wr.Write("<td></td>")
      | Some(cell) -> wr.Write($"""<td class="{if cell.Touched then "touched" else String.Empty}">{displayValue cell.Value}</td>""")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start(f)

// ----------------------------------------------------------------------------
// Helpers and continents demo
// ----------------------------------------------------------------------------

let getColumn (s:string) : int =
  int s[0] - int 'A' + 1

let raddr (s:string) =
  let col = getColumn s
  let row = int s[1..]
  col, row

let addr (s:string) =
  let m = Regex.Match(s, "^(\$?[A-Z]+)(\$?[0-9]+)$")
  let colLocation =
    if m.Groups.[1].Value.StartsWith("$") then
      m.Groups.[1].Value.Substring(1) |> getColumn |> Fixed
    else
      m.Groups.[1].Value |> getColumn |> Normal
  let rowLocation =
    if m.Groups.[2].Value.StartsWith("$") then
      m.Groups.[2].Value.Substring(1) |> int |> Fixed
    else
      m.Groups.[2].Value |> int |> Normal
  colLocation, rowLocation


let continents =
  [ "Asia", 4753079, 31033;
    "Africa", 1460481, 29648;
    "Europe", 740433, 22134;
    "North America", 604182, 21330;
    "South America", 439719, 17461;
    "Australia/Oceania", 46004, 8486;
    "Antarctica", 0, 13720 ]

let wsheet0 =
  [ yield raddr "A1", Const(String "Continent")
    yield raddr "B1", Const(String "Population (thousands)")
    yield raddr "C1", Const(String "Area (thousands km^2)")
    for i, (cont, pop, area) in Seq.indexed continents do
      yield raddr $"A{i+2}", Const(String cont)
      yield raddr $"B{i+2}", Const(Number pop)
      yield raddr $"C{i+2}", Const(Number area)
    yield raddr "A9", Const(String "World")
    yield raddr "B9", Function("SUM", [ Range(addr "B2", addr "B8") ])
    yield raddr "C9", Function("SUM", [ Range(addr "C2", addr "C8") ])
    yield raddr "D1", Const(String "Population (%)")
    yield raddr "D2", Function("/", [
        Function("*", [ Reference(addr "B2"); Const(Number 100) ])
        Reference(addr "$B$9")
      ])
    yield raddr "E1", Const(String "Area (%)")
    yield raddr "E2", Function("/", [
        Function("*", [ Reference(addr "C2"); Const(Number 100) ])
        Reference(addr "$C$9")
      ])
    yield raddr "F1", Const(String "Density (pop/km^2)")
    yield raddr "F2", Function("/", [ Reference(addr "B2"); Reference(addr "C2") ])
  ]
  |> makeSheet

// Display the initial sheet
display wsheet0
clean wsheet0

// Now expand all the calculations
let wsheet =
  wsheet0
  |> expand (raddr "D2") (raddr "D9")
  |> expand (raddr "E2") (raddr "E9")
  |> expand (raddr "F2") (raddr "F9")

// ...and display the resulting sheet!
display wsheet
clean wsheet

// And now put some aliens into Antarctica!
// (see what cells need to be recomputed)
updateNode (raddr "B8") wsheet (Const(Number(1000000)))
display wsheet
clean wsheet

