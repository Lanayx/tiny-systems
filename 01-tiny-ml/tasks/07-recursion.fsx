// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value =
  | ValNum of int
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value

and Expression =
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: A recursive definition. You can think of
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'.
  | Recursive of string * Expression * Expression

and VariableContext =
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
  Map<string, Lazy<Value>>

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
      | Some res ->
          // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
          res.Value
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
        let newCtx = c.Add(v, lazy v2)
        evaluate newCtx e
      | _ -> failwith "not a function"
  | Let(v, e1, e2) ->
    let value = evaluate ctx e1
    let newCtx = ctx.Add(v, lazy value)
    evaluate newCtx e2
  | Tuple(e1, e2) ->
      let val1 = evaluate ctx e1
      let val2 = evaluate ctx e2
      ValTuple(val1, val2)
  | TupleGet(b, e) ->
      let value = evaluate ctx e
      match value with
      | ValTuple(val1, val2) ->
          if b then
            val1
          else
            val2
      | _ -> failwith "not a tuple"
  | Match(e, v, e1, e2) ->
      let value = evaluate ctx e
      match value with
      | ValCase(b, e) ->
        let newCtx = ctx.Add(v, lazy e)
        evaluate newCtx (if b then e1 else e2)
      | _ ->
        failwith "invalid argument of match"
  | Case(b, e) ->
      ValCase(b, evaluate ctx e)
  | Recursive(v, e1, e2) ->
      // TODO: Implement recursion for 'let rec v = e1 in e2'.
      // (In reality, this will only work if 'e1' is a function
      // but the case can be implemented without assuming that).
      let rec newCtx = ctx.Add(v, lazy (evaluate newCtx e1))
      evaluate newCtx e2

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x ->
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er =
  Recursive("factorial",
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"),
        Application(Variable("factorial"),
          Binary("+", Constant(-1), Variable("x")))
      )
    )),
    Application(Variable "factorial", Constant 5)
  )
evaluate Map.empty er
