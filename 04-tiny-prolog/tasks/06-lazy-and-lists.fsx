// ----------------------------------------------------------------------------
// 06 - Lazy search and support for lists
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

let rec (|Number|_|) term =
  match term with
  | Atom "zero" -> Some 0
  | Predicate("succ", [n]) ->
      match n with
      | Number n -> Some (n + 1)
      | _ -> None
  | _ -> None


let rec (|List|_|) term : option<list<Term>> =
  // TODO: If the term represents a list, this should return the
  // elements of the list collected in an ordinary F# list.
  // If the term is 'Atom("empty")' return Some([])
  // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
  // a term representing a list 'l', return Some(h::l).
  match term with
  | Atom "empty" -> Some []
  | Predicate("cons", [h; tl]) ->
      match tl with
      | List l ->
          Some (h::l)
      | _ -> None
  | _ -> None


let rec formatTerm term =
  // TODO: Add a case for 'List(items)' - pretty print this as a list
  match term with
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | List items ->
      items |> List.map formatTerm |> String.concat "," |> sprintf "[%s]"
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


let rec solve program subst goals : seq<list<string * Term>> = seq {
  // TODO: We want to change this function to return a lazy sequence
  // of all possible substitutions solving the problem. I already
  // wrapped the code in 'seq { .. }' block for you. Change the rest
  // to recursively call 'solve' using 'yield!' and return new
  // solutions using 'yield' (replacing the printing code).
    match goals with
    | g::goals ->
        let matches = query program g
        for clause, newSubst in matches do
          let newGoals = clause.Body @ goals
          let newSubstMap = Map newSubst
          let updatedGoals = substituteTerms newSubstMap newGoals
          let updatedSubst = substituteSubst newSubstMap subst
          yield! solve program (newSubst @ updatedSubst) updatedGoals
    | [] ->
      subst
}


let run program query =
  let vars = Set.ofSeq (freeVariables query)
  for subst in solve program [] [query] do
    // TODO: To avoid cluttered output, we want to only print assignment
    // for variables that appear in the original query (and skip all
    // variables generated by the various internal matches). You can do
    // this here by iterating over variables and printing them only if
    // they are included in 'vars' (test using 'vars.Contains')
    for var, term in subst do
      if vars.Contains var then
        printf $"%s{var} = %s{formatTerm term}; "
    printfn ""
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

// Queries from previous step (now called using 'run')
run family (Predicate("father", [Variable("X"); Atom("William")]))
run family (Predicate("father", [Variable("X"); Variable("Y")]))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
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

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [num 2; num 3; Variable("X")]))
run nums (Predicate("add", [num 2; Variable("X"); num 5]))
run nums (Predicate("add", [num 2; Variable("Y"); Variable("X")]))


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

// Helper that generates a term representing a list
let rec makeList l : Term =
  // TODO: Write a helper that generates a term representing a list.
  // This should return Atom("empty") when 'l' is [] and otherwise
  // cons(t1, .. cons(tN, empty)) when 'l' is [t1; ...; tN]
  match l with
  | [] -> Atom "empty"
  | h::t -> Predicate("cons", [h; makeList t])


// TinyProlog code to represent 'append' operation on lists
// $ append([X|Y],Z,[X|W]) :- append(Y,Z,W).
// $ append([],X,X).
let append = [
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

// TODO: Test the term formatting - this should print nice outputs!
formatTerm l1to4
formatTerm l5to9
formatTerm l1to9

// Query: append([1..4], [5..9], X)
// Return: X -> [1..9]
run append (Predicate("append", [l1to4; l5to9; Variable "X"]))

// Query: append([1..4], X, [1..9])
// Return: X -> [5..9]
run append (Predicate("append", [l1to4; Variable "X"; l1to9]))

// Query: append(X, Y, [1..9])
// Return:
//  * X -> [1..9], Y -> []
//  * X -> [1..8], Y -> [9]
//  * X -> [1..7], Y -> [8, 9]
//  * X -> [1..6], Y -> [7 .. 9]
//  * etc.
run append (Predicate("append", [Variable "Y"; Variable "X"; l1to9]))
