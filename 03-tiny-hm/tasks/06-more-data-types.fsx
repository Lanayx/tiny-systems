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
  // NOTE: Added two types of expression for working with unions
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

type Type =
  | TyVariable of string
  | TyBool
  | TyNumber
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  // NOTE: Added type for tuples
  | TyUnion of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  match ty with
  | TyVariable(v) -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList(ty) -> occursCheck vcheck ty
  | TyFunction(ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2
  | TyTuple(ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2
  | TyUnion(ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2

let rec substType (subst:Map<_, _>) t1 =
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  match t1 with
  | TyVariable(v) ->
    match subst |> Map.tryFind v with
    | Some(ty) -> ty
    | None -> t1
  | TyBool -> TyBool
  | TyNumber -> TyNumber
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
      // TODO: As with tuples, we know the type of 'e' is some union,
      // but we do not know what. We need new type variables. When
      // checking 'e1' and 'e2', add variable 'v' to the context!
      // Also note that the return types of 'e1' and 'e2' have to match.
      let t1, s1 = generate ctx e
      let tLeft = newTyVariable()
      let tRight = newTyVariable()
      let t2, s2 = generate (ctx |> Map.add v tLeft) e1
      let t3, s3 = generate (ctx |> Map.add v tRight) e2
      t2, s1 @ s2 @ s3 @ [ t1, TyUnion(tLeft, tRight); t2, t3 ]

  | Case(b, e) ->
      // TODO: Here, we know the type of 'e' is the type of one of
      // the cases, but we still need a new type variable for the other.
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



// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e =
  let typ, constraints = generate Map.empty e
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Both cases are constrained because 'if' returns either one or the other
// * fun x -> if x = 0 then Case1(fun x -> x) else Case2(42)
Lambda("x",
  If(Binary("=", Variable("x"), Constant(0)),
    Case(true, Lambda("x", Variable("x"))),
    Case(false, Constant(42))
  ))
|> infer

// No constraints to fix the second case type (case<number, 'a> -> number)
// * fun x -> match x with Case1 v -> v + 1 | Case2 _ -> 0
Lambda("x", Match(Variable("x"), "v",
  Binary("+", Variable("v"), Constant(1)),
  Constant(0)))
|> infer