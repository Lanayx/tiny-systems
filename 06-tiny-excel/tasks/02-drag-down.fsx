// ----------------------------------------------------------------------------
// 02 - "Drag down" formula expanding
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

type Sheet = Map<Address, Expr>

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) =
  // TODO: Replace references in expression 'srcExpr' in a way that
  // corresponds to moving the expression from address (srcRow, srcCol)
  // to address (tgtRow, tgtCol). So for example, if a formula 'A1+A2' is
  // moved from 'A3' to 'B10' then it should change to 'B8+B9' (address
  // is incremented by column difference 1 and row difference 7)
  match srcExpr with
  | Const v -> Const v
  | Reference (col, row) -> Reference (col + tgtCol - srcCol, row + tgtRow - srcRow)
  | Function (name, args) -> Function (name, List.map (relocateReferences (srcCol, srcRow) (tgtCol, tgtRow)) args)


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:Sheet) : Sheet =
  // TODO: Expand formula at address (srcCol, srcRow) to all the cells
  // between itself and target cell at address (tgtCol, tgtRow) and
  // add the new formulas to the given sheet, returning the new sheet.
  //
  // HINT: You can use list comprehension with 'for .. in .. do' and
  // 'yield' or you can use 'List.init'. The comprehension is nicer,
  // but you need to figure out the right syntax! Once you generate
  // new cells, you can add them to the Map using List.fold (with the
  // sheet as the current state, updated in each step using Map.add).
  [ for i in srcRow..tgtRow do
      for j in srcCol..tgtCol do
        yield (j, i), relocateReferences (srcCol, srcRow) (j, i) (Map.find (srcCol, srcRow) sheet) ]
  |> List.fold (fun s (k, v) -> Map.add k v s) sheet


// ----------------------------------------------------------------------------
// Simple recursive evaluator
// ----------------------------------------------------------------------------

let rec eval (sheet:Sheet) expr =
  match expr with
  | Const(v) -> v
  | Reference(a) ->
    match Map.tryFind a sheet with
    | Some(e) -> eval sheet e
    | None -> Error "Missing value"
  | Function("+", [a; b]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) -> Number(x + y)
    | _ -> Error "Invalid arguments"
  | Function("*", [a; b]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) -> Number(x * y)
    | _ -> Error "Invalid arguments"
  | Function(name, _) -> Error $"Unknown function: {name}"


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) =
  let col = int s[0] - int 'A' + 1
  let row = int s[1..]
  col, row


let fib =
  [ addr "A1", Const(Number 0)
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> Map.ofList
  |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))

// Should return: Number 21
eval fib (Reference(addr "A9"))

// Should return: Number 34
eval fib (Reference(addr "A10"))

// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


// Column 'A' is a sequence of numbers increasing by 1
// Column 'B' is the factorial of the corresponding number
// i.e.: Bn = An * B(n-1) = An * A(n-1)!
let fac =
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ]
  |> Map.ofList
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")

// A6 should be 5, B6 should be 120
eval fac (Reference(addr "A6"))
eval fac (Reference(addr "B6"))

// A11 should be 10, B11 should be 3628800
eval fac (Reference(addr "A11"))
eval fac (Reference(addr "B11"))
