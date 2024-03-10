// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
// ----------------------------------------------------------------------------

type Address = int * int

type Value =
  | Number of int
  | String of string
  | Error of string
  override this.ToString() =
    match this with
    | Number(n) -> $"number {n}"
    | String(s) -> $"string {s}"
    | Error(e) -> $"error {e}"

type Expr =
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

type CellNode =
  { mutable Value : Value
    mutable Expr : Expr
    // NOTE: Added event that will be triggered when the
    // expression and value of the node is changed.
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
  // TODO: Collect the addresses of all references that appear in the
  // expression 'expr'. This needs to call itself recursively for all
  // arguments of 'Function' and concatenate the returned lists.
  // HINT: This looks nice if you use 'List.collect'.
  match expr with
  | Const(_) -> []
  | Reference(a) -> [a]
  | Function(_, args) ->
     match args with
      | [] -> []
      | _ -> List.collect collectReferences args


let makeNode (sheet:LiveSheet) expr =
  // TODO: Add handling of 'Update' events!
  //
  // * When creating a node, we need to create a new event and
  //   set it as the 'Updated' event of the returned node.
  // * We then need to define 'update' function that will be triggered
  //   when any of the cells on which this one depends change. In the
  //   function, re-evaluate the formula, set the new value and trigger
  //   our Updated event to notify other cells.
  // * Before returning, use 'collectReferences' to find all cells on which
  //   this one depends and add 'update' as the handler of their
  //   'Updated' event
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
  // TODO: For now, we ignore the fact that the new expression may have
  // different set of references than the one we are replacing.
  // So, we can just get the node, set the new expression and value
  // and trigger the Updated event!
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
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) =
  let col = int s[0] - int 'A' + 1
  let row = int s[1..]
  col, row


// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
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
    // TODO: Add formula for Celsius to Fahrenheit conversion to 'C2'
    addr "C2",
      Function("+", [
        Function("/", [
          Function("*", [ Reference(addr "B2"); Const(Number 9) ])
          Const(Number 5) ])
        Const(Number 32) ])
  ]
  |> makeSheet

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1"))
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1"))
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1"))

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2"))
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2"))
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2"))

