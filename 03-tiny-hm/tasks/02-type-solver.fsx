// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------


// NOTE: We will only need lists later, but to make this exercise
// a bit more interesting, we will implement constraint resolution
// for lists here already. This will help you in the next steps!
type Type =
  | TyVariable of string
  | TyBool
  | TyNumber
  | TyList of Type

let rec occursCheck vcheck ty =
  // TODO: Return true of type 'ty' contains variable 'vcheck'
  match ty with
  | TyVariable(v) -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList(ty) -> occursCheck vcheck ty

let rec substType (subst:Map<string, Type>) ty =
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyVariable(v) ->
    match subst |> Map.tryFind v with
    | Some(ty) -> ty
    | None -> ty
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyList(ty) -> TyList(substType subst ty)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) =
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
  cs |> List.map (fun (n1, n2) -> (substType subst n1, substType subst n2))

let substituteAll (subst:list<string*Type>) (ty:Type) =
  subst |> List.fold (fun ty (v, subst) -> substType (Map.empty.Add(v, subst)) ty) ty

let solve cs =
  let rec solveInner vars cs =
    match cs with
    | [] -> []
    | (TyNumber, TyNumber)::cs -> solveInner vars cs
    // TODO: Fill in the remaining cases! You can closely follow the
    // example from task 1 - the logic here is exactly the same.
    | (TyBool, TyBool)::cs -> solveInner vars cs
    | (TyList(ty1), TyList(ty2))::cs -> solveInner vars ((ty1, ty2)::cs)
    | (ty, TyVariable(v))::cs
    | (TyVariable(v), ty)::cs ->
      if occursCheck v ty then failwith "occurs check failed"
      let newMap = vars |> Map.add v ty
      let cs = substConstrs newMap cs
      let subst = solveInner newMap cs
      let ty = substituteAll subst ty
      (v, ty)::subst
    | _ -> failwith "cannot solve"
  solveInner Map.empty cs



// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)

solve
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve
  [ TyList(TyVariable("a")), TyVariable("a") ]
