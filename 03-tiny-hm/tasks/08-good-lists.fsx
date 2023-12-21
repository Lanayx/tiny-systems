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
  | Recursive of string * Expression * Expression
  | Unit
  // NOTE: To keep things simpler, we add special expressions
  // for list construction and pattern matching on lists.
  | ListCase of bool * Expression
  | ListMatch of Expression * string * Expression * Expression

type Type =
  | TyVariable of string
  | TyBool
  | TyNumber
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  | TyUnion of Type * Type
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

let solve constraints =
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
      if occursCheck v ty then
          printfn "%A:%A" v ty
          failwith "occurs check failed"
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
      TyUnit, []
  | Recursive(v, e1, e2) ->
      let var = newTyVariable()
      let t1, s1 = generate (ctx |> Map.add v var) e1
      let t2, s2 = generate (ctx |> Map.add v var) e2
      t2, s1 @ s2 @ [ var, t1 ]

  | ListMatch(e, v, e1, e2) ->
      // TODO: Type of 'e' ('tylist') needs to be a list of elements ('tyel').
      // In 'e1', the type of the variable 'v' is then a tuple 'tyel * tylist'.
      // In 'e2', the type of the variable 'v' is just 'unit'.
      // To express this, you will need a new type variable for 'tyel'.
      let tyel = newTyVariable()
      let tylist, s1 = generate ctx e
      let t2, s2 = generate (ctx |> Map.add v (TyTuple(tyel, tylist))) e1
      let t3, s3 = generate (ctx |> Map.add v TyUnit) e2
      t2, s1 @ s2 @ s3 @ [tylist, TyList(tyel); t2, t3]

  | ListCase(true, Tuple(ehd, etl)) ->
      // TODO: If type of 'ehd' is 'tyel' and type of 'etl' is 'tylist'
      // then we need a constraint 'tylist = list<tyel>'.
      let tyel, s1 = generate ctx ehd
      let tylist, s2 = generate ctx etl
      tylist, s1 @ s2 @ [tylist, TyList(tyel)]

  | ListCase(false, Unit) ->
      // TODO: The type of '[]' is a list of some type (needs a type variable)
      let tv = newTyVariable()
      TyList(tv), []

  | ListCase _ ->
      // TODO: For simplicity, we here restrict the syntax of list constructs.
      // In general, this is not needed, but it makes the task easier...
      failwith "unsupported list syntax"

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e =
  let typ, constraints = generate Map.empty e
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// NOTE: The following is modified from task 7 to use
// ListCase and ListMatch instead of normal Case and Match.
// It should all type check as expected now!

let rec makeListExpr l =
  match l with
  | x::xs -> ListCase(true, Tuple(x, makeListExpr xs))
  | [] -> ListCase(false, Unit)

makeListExpr [ for i in 1 .. 5 -> Constant i ]
|> infer

// doesn't work :(
Recursive("map",
  Lambda("f", Lambda("l",
    ListMatch(
      Variable("l"), "x",
      ListCase(true, Tuple(
        Application(Variable "f", TupleGet(true, Variable "x")),
        Application(Application(Variable "map", Variable "f"),
          TupleGet(false, Variable "x"))
      )),
      ListCase(false, Unit)
    )
  )),
  Variable("map"))
|> infer
