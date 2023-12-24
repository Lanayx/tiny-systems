// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------


// TODO: Write an active pattern to recognize numbers in the form used below.
// If the term is 'Atom("zero")' return Some(0).
// If the term is 'Predicate("succ", [n])' where 'n' is itself
// a term representing number, return the number value +1.
let rec (|Number|_|) term =
  match term with
  | Atom "zero" -> Some 0
  | Predicate("succ", [n]) ->
      match n with
      | Number n -> Some (n + 1)
      | _ -> None
  | _ -> None

let rec formatTerm term =
  match term with
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      items |> List.map formatTerm |> String.concat "," |> sprintf "%s(%s)" p

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

let rec solve program subst goals =
  match goals with
  | g::goals ->
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = clause.Body @ goals
        let newSubstMap = Map newSubst
        let updatedGoals = substituteTerms newSubstMap newGoals
        let updatedSubst = substituteSubst newSubstMap subst
        solve program (newSubst @ updatedSubst) updatedGoals
  | [] ->
    // TODO: When printing the computed substitution 'subst', print
    // the terms nicely using 'formatTerm'. You can use 'for' loop like:
    // 'for var, term in subst do printfn ...'
    for var, term in subst do
      if var.Length = 1 then
        printf $"%s{var} = %s{formatTerm term}; "
    do printfn ""


// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  if n = 0 then
    Atom("zero")
  else
    Predicate("succ", [num (n - 1)])


// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5'
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3'
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))'
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
