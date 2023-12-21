// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression =
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: Added the unit value and recursive definition
  | Recursive of string * Expression * Expression
  | Unit

type Type =
  | TyVariable of string
  | TyBool
  | TyNumber
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  | TyUnion of Type * Type
  // NOTE: We need another primitive type for units
  | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
  match ty with
  | TyVariable(v) -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyUnit -> false
  | TyList(ty) -> occursCheck vcheck ty
  | TyFunction(ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2
  | TyTuple(ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2
  | TyUnion(ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2

let rec substType (subst:Map<_, _>) t1 =
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
  match t1 with
  | TyVariable(v) ->
    match subst |> Map.tryFind v with
    | Some(ty) -> ty
    | None -> t1
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyUnit -> TyUnit
  | TyList(ty) -> TyList(substType subst ty)
  | TyFunction(ty1, ty2) -> TyFunction(substType subst ty1, substType subst ty2)
  | TyTuple(ty1, ty2) -> TyTuple(substType subst ty1, substType subst ty2)
  | TyUnion(ty1, ty2) -> TyUnion(substType subst ty1, substType subst ty2)

let substConstrs subst cs =
  cs |> List.map (fun (n1, n2) -> (substType subst n1, substType subst n2))

let substituteAll (subst:list<string*Type>) (ty:Type) =
  subst |> List.fold (fun ty (v, subst) -> substType (Map.empty.Add(v, subst)) ty) ty

let rec solve constraints =
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  let rec solveInner vars cs =
    match cs with
    | [] -> []
    | (TyNumber, TyNumber)::cs -> solveInner vars cs
    | (TyBool, TyBool)::cs -> solveInner vars cs
    | (TyUnit, TyUnit)::cs -> solveInner vars cs
    | (TyList(ty1), TyList(ty2))::cs -> solveInner vars ((ty1, ty2)::cs)
    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs -> solveInner vars ((ta1, ta2)::(tb1, tb2)::cs)
    | (TyTuple(ty1, ty2), TyTuple(ty3, ty4))::cs -> solveInner vars ((ty1, ty3)::(ty2, ty4)::cs)
    | (TyUnion(ty1, ty2), TyUnion(ty3, ty4))::cs -> solveInner vars ((ty1, ty3)::(ty2, ty4)::cs)
    | (ty, TyVariable(v))::cs
    | (TyVariable(v), ty)::cs ->
      if occursCheck v ty then failwith "occurs check failed"
      let newMap = vars |> Map.add v ty
      let cs = substConstrs newMap cs
      let subst = solveInner newMap cs
      let ty = substituteAll subst ty
      (v, ty)::subst
    | _ -> failwith "cannot solve"
  solveInner Map.empty constraints


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable =
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e =
  match e with
  | Constant _ ->
      TyNumber, []
  | Binary("*", e1, e2)
  | Binary("+", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op
  | Variable v ->
      ctx[v], []
  | If(econd, etrue, efalse) ->
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse
      t2, s1 @ s2 @ s3 @ [ t1, TyBool; t2, t3 ]
  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate (ctx |> Map.add v t1) e2
      t2, s1 @ s2
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let t1, s1 = generate (ctx |> Map.add v targ) e
      TyFunction(targ, t1), s1
  | Application(e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      let targ = newTyVariable()
      let tret = newTyVariable()
      tret, s1 @ s2 @ [ t1, TyFunction(targ, tret); t2, targ ]
  | Tuple(e1, e2) ->
    let t1, s1 = generate ctx e1
    let t2, s2 = generate ctx e2
    TyTuple(t1, t2), s1 @ s2
  | TupleGet(b, e) ->
      let v1 = newTyVariable()
      let v2 = newTyVariable()
      match b with
      | true ->
          let t1, s1 = generate ctx e
          v1, s1 @ [ t1, TyTuple(v1, v2) ]
      | false ->
          let t1, s1 = generate ctx e
          v2, s1 @ [ t1, TyTuple(v1, v2) ]
  | Match(e, v, e1, e2) ->
      let t1, s1 = generate ctx e
      let tLeft = newTyVariable()
      let tRight = newTyVariable()
      let t2, s2 = generate (ctx |> Map.add v tLeft) e1
      let t3, s3 = generate (ctx |> Map.add v tRight) e2
      t2, s1 @ s2 @ s3 @ [ t1, TyUnion(tLeft, tRight); t2, t3 ]
  | Case(b, e) ->
      let t1, s1 = generate ctx e
      match b with
      | true ->
        let right = newTyVariable()
        let tResult = TyUnion(t1, right)
        tResult, s1
      | false ->
        let left = newTyVariable()
        let tResult = TyUnion(left, t1)
        tResult, s1

  | Unit ->
      // NOTE: This is so easy I wrote it for you :-)
      TyUnit, []

  | Recursive(v, e1, e2) ->
      // TODO: This is easier than evaluation. We need a new type variable
      // for the type of the thing we are defining (variable 'v') and add
      // it to the context when checking both 'e1' and 'e2'.
      let var = newTyVariable()
      let t1, s1 = generate (ctx |> Map.add v var) e1
      let t2, s2 = generate (ctx |> Map.add v var) e2
      t2, s1 @ s2 @ [ var, t1 ]


// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e =
  let typ, constraints = generate Map.empty e
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Helper to generate list 1 .. 5 from TinyML tasks
let rec makeListExpr l =
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

// We can type check this, but the type is horrible!
makeListExpr [ for i in 1 .. 5 -> Constant i ]
|> infer

// Code for the List.map function from TinyML task. This fails to check.
Recursive("map",
  Lambda("f", Lambda("l",
    Match(
      Variable("l"), "x",
      Case(true, Tuple(
        Application(Variable "f", TupleGet(true, Variable "x")),
        Application(Application(Variable "map", Variable "f"),
          TupleGet(false, Variable "x"))
      )),
      Case(false, Unit)
    )
  )),
  Variable("map"))
|> infer
