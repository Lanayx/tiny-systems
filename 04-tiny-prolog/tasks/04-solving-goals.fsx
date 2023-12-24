// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

type Term =
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) (term: Term) =
  match term with
  | Atom _ -> term
  | Variable var ->
    match subst.TryFind(var) with
    | Some term -> term
    | None -> term
  | Predicate (name, terms) ->
      Predicate (name, List.map (substitute subst) terms)

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) =
  subst |> List.map (fun (var, term) -> (var, substitute newSubst term))

let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) =
  terms |> List.map (substitute subst)

let rec unifyLists l1 l2 =
  match l1, l2 with
  | [], [] ->
      Some []
  | h1::t1, h2::t2 ->
      let s1 = unify h1 h2
      match s1 with
      | None -> None
      | Some s1 ->
          let s1map = Map(s1)
          let t1x = substituteTerms s1map t1
          let t2x = substituteTerms s1map t2
          let s2 = unifyLists t1x t2x
          match s2 with
          | None -> None
          | Some s2 ->
              let s2map = Map(s2)
              let s1x = substituteSubst s2map s1
              Some (s1x @ s2)
  | _ -> None

and unify t1 t2 =
  match t1, t2 with
  | Atom a1, Atom a2 ->
      if a1 = a2 then Some [] else None
  | Predicate (p1, l1), Predicate (p2, l2) ->
      if p1 = p2 then unifyLists l1 l2 else None
  | Variable v, x
  | x, Variable v ->
      Some [v, x]
  | _ ->
      None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------
let nextNumber =
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables (term: Term) =
  match term with
  | Atom _ -> []
  | Variable var -> [var]
  | Predicate (name, terms) ->
      terms |> List.collect freeVariables

let withFreshVariables (clause:Clause) : Clause =
  let varMap =
    clause.Head :: clause.Body
    |> List.collect freeVariables
    |> List.distinct
    |> List.map (fun var -> var, var + nextNumber().ToString())
    |> Map
  let rec substituteTerm (term:Term) =
    match term with
    | Atom _ -> term
    | Variable var -> Variable <| varMap[var]
    | Predicate (name, terms) ->
        Predicate (name, List.map substituteTerm terms)
  {
    Head = clause.Head |> substituteTerm
    Body = clause.Body |> List.map substituteTerm
  }

let query (program:list<Clause>) (query:Term) =
  let rec queryClause (clause:Clause) =
    let clause = withFreshVariables clause
    match unify clause.Head query with
    | Some subst -> Some (clause, subst)
    | None -> None
  program |> List.choose queryClause

let rec solve program subst (goals: Term list) =
  match goals with
  | g::goals ->
      // TODO: We need to solve the goal (term) 'g'. To do so, find all
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = clause.Body @ goals
        let newSubstMap = Map newSubst
        let updatedGoals = substituteTerms newSubstMap newGoals
        let updatedSubst = substituteSubst newSubstMap subst
        solve program (newSubst @ updatedSubst) updatedGoals
  | [] ->
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    printfn "%A" subst

// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

// Some information about the British royal family
let family = [
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]

