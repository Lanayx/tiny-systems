// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value =
  | ValNum of int
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression =
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

and VariableContext =
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with
      | ValNum n1, ValNum n2 ->
          match op with
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following from before
  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
      | ValNum n ->
        match op with
        | "-" -> ValNum(-n)
        | _ -> failwith "unsupported unary operator"
      | _ -> failwith "unsupported value for unary operator"
  | If(cond, cont1, cont2) ->
      let v1 = evaluate ctx cond
      match v1 with
      | ValNum n1 ->
        if n1 = 1 then
          evaluate ctx cont1
        else
          evaluate ctx cont2
      | _ -> failwith "unsupported value for if"
  | Lambda(v, e) ->
      ValClosure(v, e, ctx)
  | Application(e1, e2) ->
      let v1 = evaluate ctx e1
      match v1 with
      | ValClosure(v, e, c) ->
        let v2 = evaluate ctx e2
        let newCtx = c.Add(v, v2)
        evaluate newCtx e
      | _ -> failwith "not a function"
  | Let(v, e1, e2) ->
    let value = evaluate ctx e1
    let newCtx = ctx.Add(v, value)
    evaluate newCtx e2

  | Tuple(e1, e2) ->
      let val1 = evaluate ctx e1
      let val2 = evaluate ctx e2
      ValTuple(val1, val2)

  | TupleGet(b, e) ->
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
      let value = evaluate ctx e
      match value with
      | ValTuple(val1, val2) ->
          if b then
            val1
          else
            val2
      | _ -> failwith "not a tuple"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1=
  TupleGet(true,
    Tuple(Binary("*", Constant(2), Constant(21)),
      Constant(123)))
evaluate Map.empty ed1

let ed2 =
  TupleGet(false,
    Tuple(Binary("*", Constant(2), Constant(21)),
      Constant(123)))
evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 =
  TupleGet(true, Constant(42))
evaluate Map.empty ed3
