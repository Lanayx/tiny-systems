// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
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
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  match term with
  | Atom _ -> []
  | Variable var -> [var]
  | Predicate (name, terms) ->
      terms |> List.collect freeVariables


let withFreshVariables (clause:Clause) : Clause =
  // TODO: Get a list of distinct variables in the clause (using
  // 'freeVariables' and 'List.distinct'), generate a substitution
  // that append a number 'n' obtained by 'nextNumber()' to the end
  // of all the variable names, and apply the substitutions to the
  // head and body of the clause.
  //
  // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
  // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
  //
  // This may not be correct if the user-provided names of variables
  // had numbers in them in a certain format, but that's OK for now!

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

let query (program:list<Clause>) (query:Term)
    : list<Clause * list<string * Term>> =
  // TODO: Return all clauses from 'program' whose 'Head' can be
  // unified with the specified 'query' and return the resulting
  // substitutions. Before unifying, rename variables in the program
  // rule using 'withFreshVariables'. You can do this using 'List.choose'
  // or by using list comprehension.
  //
  // The return type of this is a list of tuples consisting of the matching
  // clause and a substitution (list<string * Term>). Calling 'unify'
  // gives you 'option<list<string * Term>>', so you need to pattern match
  // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.

  let rec queryClause (clause:Clause) =
    let clause = withFreshVariables clause
    match unify clause.Head query with
    | Some subst -> Some (clause, subst)
    | None -> None

  program |> List.choose queryClause

// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

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

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
