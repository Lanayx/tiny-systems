// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
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
  match term with
  | Atom "empty" -> Some []
  | Predicate("cons", [h; tl]) ->
      match tl with
      | List l ->
          Some (h::l)
      | _ -> None
  | _ -> None

let rec formatTerm term =
  match term with
  | Number n -> string n
  | List items ->
      items |> List.map formatTerm |> String.concat "," |> sprintf "[%s]"
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
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

let permutation =
  append @ [
    fact (Predicate("perm", [ Atom("empty"); Atom("empty") ]))
    rule (Predicate("perm", [ Variable("L"); Predicate("cons", [Variable("H"); Variable("T")]) ])) [
      Predicate("append", [ Variable("V"); Predicate("cons", [Variable("H"); Variable("U")]); Variable("L") ])
      Predicate("append", [ Variable("V"); Variable("U"); Variable("W") ])
      Predicate("perm", [ Variable("W"); Variable("T") ])
    ]
  ]

// DEMO: Generate all permutations of the list [1 .. 4]
run permutation (Predicate("perm", [l1to4; Variable("X")]))


// ----------------------------------------------------------------------------
// Generating magic squares
// ----------------------------------------------------------------------------

// Custom operator and a hlper function for equality & defining variables
let (.=.) a b = Predicate("eq", [a; b])
let var x = Variable(x)

// TinyProlog is too slow! But if we give it the numbers in an order
// that is close to being a magic square (first row is correct), it will
// manage to generate a magic square sooner or later...
let l = [ 2;7;6; 1;3;4; 5;8;9 ]

let magic = permutation @ nums @ [
  rule (Predicate("add3", [ var "A"; var "B"; var "C"; var "S" ])) [
    Predicate("add", [ var "A"; var "B"; var "T" ])
    Predicate("add", [ var "T"; var "C"; var "S" ])
  ]
  rule (Predicate("magic", [ var "S"; var "X" ])) [
    yield Predicate("perm", [makeList [ for i in l -> num i ]; var "X"])
    yield var "X" .=. makeList [ var "A1"; var "A2"; var "A3"; var "B1";
      var "B2"; var "B3"; var "C1"; var "C2"; var "C3" ]
    for a, b, c in [
      ("A1","A2","A3"); ("B1","B2","B3"); ("C1","C2","C3")
      ("A1","B1","C1"); ("A2","B2","C2"); ("A3","B3","C3")
      ("A1","B2","C3"); ("A3","B2","C1") ] do
      yield Predicate("add3", [var a; var b; var c; var "S"])
  ]
]

run magic (Predicate("magic", [num 15; var "X"]))
