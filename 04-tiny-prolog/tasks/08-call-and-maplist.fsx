// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term =
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  // NOTE: Added 'Call' as a special kind of predicate
  | Call of Term * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term =
  // TODO: Add a case for 'Call' to substitution
  match term with
  | Atom _ -> term
  | Variable var ->
    match subst.TryFind(var) with
    | Some term -> term
    | None -> term
  | Predicate (name, terms) ->
      Predicate (name, List.map (substitute subst) terms)
  | Call (term, terms) ->
      Call (substitute subst term, List.map (substitute subst) terms)

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
  // TODO: This is where we need a clever trick to handle 'Call'!
  // Unification can succeed if we have a predicate and a call with a
  // corresponding predicate as the first argument. So we can unify:
  //
  //   Predicate(p1, args1) ~ Call(Predicate(p2, args2a), args2b)
  //
  // When 'p1 = p2' and when we can unify 'args1 ~ args2a @ args2b'.
  match t1, t2 with
  | Atom a1, Atom a2 ->
      if a1 = a2 then Some [] else None
  | Predicate (p1, l1), Predicate (p2, l2) ->
      if p1 = p2 then unifyLists l1 l2 else None
  | Variable v, x
  | x, Variable v ->
      Some [v, x]
  | Call (Predicate(p2, args2a), args2b), Predicate(p1, args1)
  | Predicate(p1, args1), Call (Predicate(p2, args2a), args2b) ->
      if p1 = p2 then
        unifyLists args1 (args2a @ args2b)
      else
        None
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
  match term with
  | Atom "empty" -> Some []
  | Predicate("cons", [h; tl]) ->
      match tl with
      | List l ->
          Some (h::l)
      | _ -> None
  | _ -> None

let rec formatTerm term =
  // TODO: You can format 'Call(args)' as 'Predicate("call", args)'
  match term with
  | Number n -> string n
  | List items ->
      items |> List.map formatTerm |> String.concat "," |> sprintf "[%s]"
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      items |> List.map formatTerm |> String.concat "," |> sprintf "%s(%s)" p
  | Call(predicate, args) ->
      let predicateFormat = formatTerm predicate
      let callFormat = formatTerm <| Predicate("call", args)
      $"{callFormat}{{{predicateFormat}}}"

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber =
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term =
  // TODO: Add a case for 'Call' when getting free variables
  match term with
  | Atom _ -> []
  | Variable var -> [var]
  | Predicate (name, terms) ->
      terms |> List.collect freeVariables
  | Call (term, terms) ->
      freeVariables term @ (terms |> List.collect freeVariables)

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
    | Call (term, terms) ->
        Call (substituteTerm term, List.map substituteTerm terms)
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
    for var, term in subst do
      if vars.Contains var then
        printf $"%s{var} = %s{formatTerm term}; "
    printfn ""

// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n =
  if n = 0 then
    Atom("zero")
  else
    Predicate("succ", [num (n - 1)])

let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term =
  match l with
  | [] -> Atom "empty"
  | h::t -> Predicate("cons", [h; makeList t])

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

// ----------------------------------------------------------------------------
// Call and maplist
// ----------------------------------------------------------------------------

// The Prolog 'call' operation takes a term and a list of arguments
// and supplies the arguments as additional arguments to the term.
// So, for example, calling 'call(add(1), 2, X)' becomes 'add(1, 2, X)'
run nums (Call(Predicate("add", [num 1]), [num 2; Variable "X"]))
run nums (Call(Predicate("add", [num 1; Variable "X"]), [num 5]))

// This can be used to implement the 'maplist' function:
// $ maplist(_, [], []).
// $ maplist(G,[X|Xs],[Y|Ys]) :- maplist(G,Xs,Ys), call(G,X,Y).
let maplist = [
  fact (Predicate("maplist", [ Variable("_"); Atom("empty"); Atom("empty") ]))
  rule (Predicate("maplist", [
    Variable("G")
    Predicate("cons", [ Variable("X"); Variable("Xs") ])
    Predicate("cons", [ Variable("Y"); Variable("Ys") ]);
  ])) [
    Predicate("maplist", [ Variable("G"); Variable("Xs"); Variable("Ys") ])
    Call(Variable("G"), [ Variable("X"); Variable("Y") ])
  ]
]

// Query: maplist(add(10), l1to9, Y)
// Returns: Y -> [11; 12; ..; 19]
run (nums @ maplist) (Predicate("maplist",
  [ Predicate("add", [num 10]); l1to9; Variable("Y") ]))