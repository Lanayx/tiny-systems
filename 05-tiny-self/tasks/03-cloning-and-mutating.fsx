// ----------------------------------------------------------------------------
// 03 - Cloning and mutating TinySelf objects
// ----------------------------------------------------------------------------

type Slot =
  { Name : string
    Contents : Objekt
    IsParent : bool }

and Objekt =
  { mutable Slots : Slot list
    mutable Code : Objekt option
    mutable Special : Special option }

and Special =
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeObject slots code =
  { Code = Some code; Special = None; Slots = slots }
let makeDataObject slots =
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject slots special =
  { Code = None; Special = Some special; Slots = slots }

let makeSlot n contents =
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot n contents =
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

// NOTE: Implemented in step #2
let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit =
  let newSlot = makeSlot n contents
  obj.Slots <- newSlot :: obj.Slots

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit =
  let newSlot = makeParentSlot n contents
  obj.Slots <- newSlot :: obj.Slots

let cloneObject (obj:Objekt) : Objekt =
  { Slots = obj.Slots; Code = obj.Code; Special = obj.Special }

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

// TODO: To implement assignment, we need to know what object a slot
// comes from. Modify 'lookup' so that it returns not just the slot,
// but also the object that the slot comes from.
let rec lookup (msg:string) (obj:Objekt) : list<Objekt * Slot> =
  let msgSlots = obj.Slots |> List.filter (fun s -> s.Name = msg)
  if not msgSlots.IsEmpty then
    msgSlots |> List.map (fun s -> obj, s)
  else
    parentLookup msg obj
and parentLookup msg obj: list<Objekt * Slot> =
  match obj.Slots |> List.filter (fun s -> s.IsParent) with
  | [] -> []
  | ps -> ps |> List.map (fun s -> lookup msg s.Contents) |> List.concat

// TODO: Modify 'send' and 'eval' to also take message send arguments.
// In Self, the arguments are copied into the activation record.
// In TinySelf, we use simpler trick - just make the 'args' object
// another parent of the activation record! Lookup for argument name
// in the activation record will then give us the value.
// NOTE: The object newly returned from 'lookup' should be ignored.
// BEWARE: All arguments are 'Objekt' so it is easy to swap them!!
let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
  | None -> slotValue
  | Some code ->
    match code.Special with
    | Some(Native f) ->
      let ar = cloneObject code
      ar |> addParentSlot "self*" instance
      ar |> addParentSlot "new" args
      f ar
    | _ -> failwith "not implemented!"
let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  let slots = lookup msg instance |> List.map snd
  match slots with
  | [ { Contents = it } ] -> eval it args instance
  | _ ->
    failwithf "send: Expected exactly one slot '%s' (found %d)! insance: %A" msg slots.Length instance

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o =
  match lookup n o with
  // NOTE: We ignore the object returned by 'lookup' here.
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o =
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// NOTE: Implemented in step #2
let empty : Objekt = { Slots = []; Code = None; Special = None }
let printCode = makeNativeMethod (fun arcd ->
  match arcd |> send "value" empty with
  | { Special = Some(String s) } -> printfn "%s" s
  | _ -> failwith "not a string value"
  empty
)
let stringPrototype = makeDataObject [
  makeSlot "print" printCode
]
let makeString s =
  makeDataObject [
    makeSlot "value" (makeSpecialObject [] (String s))
    makeParentSlot "parent*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Cloning and assignments
// ----------------------------------------------------------------------------

let cloneMethod = makeNativeMethod (fun arcd ->
  // TODO: The activation record contains a slot 'self*' which is the
  // target object. Use lookup to get it, clone it & retrn it!
  // (If the lookup returns a wrong thing, fail - that's wrong.)
  match lookup "self*" arcd with
  | [ _, { Contents = it } ] -> it |> cloneObject
  | _ -> failwith "wrong thing to clone"
)

let clonablePrototype =
  // TODO: Create an object that has the 'clone' method
  { Slots = [ makeSlot "clone" cloneMethod ]; Code = None; Special = None }

let assignmentMethod n = makeNativeMethod (fun arcd ->
  // TODO: The activation record has a slot named 'n' somewhere in its
  // inheritance graph and a slot 'new' which is a method argument.
  // Find those two using 'lookup' and modify the slot value (in the
  // that contained it - as returned from lookup). (Tiny)Self assignment
  // should return the object that has been modified.
  match lookup n arcd with
  | [ _, { Contents = it } ] ->
    let argsSlotValue = lookupSlotValue "new" arcd
    it.Code <- argsSlotValue.Code
    it.Special <- argsSlotValue.Special
    it.Slots <- argsSlotValue.Slots
    it

  | _ -> failwith "wrong target to assign"
)

// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n =
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// Tests - cloning and modifying cats
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let mogscats = makeDataObject [
  makeSlot "book" (makeString "Mog's Family of Cats")
  // NOTE: This allows us to rename the book (probably not
  // something you'd want to do, but for illustration purposes...)
  makeAssignmentSlot "book"
]
let mog = makeDataObject [
  // NOTE: Mog is now also clonable and has assignment slot "name:"
  makeParentSlot "parent*" cat
  makeParentSlot "clonable*" clonablePrototype
  makeParentSlot "fictional*" mogscats
  makeSlot "name" (makeString "Mog")
  makeAssignmentSlot "name"
]

// NOTE: We now pass empty arguments to all of the message sends
mog |> send "name" empty |> send "print" empty |> ignore
mog |> send "sound" empty |> send "print" empty |> ignore
mog |> send "book" empty |> send "print" empty |> ignore

// NOTE: Clone Ginger and print its name & book
let ginger = mog |> send "clone" empty

ginger |> send "name" empty |> send "print" empty |> ignore
ginger |> send "book" empty |> send "print" empty |> ignore

// TODO: Write code to change the name of 'ginger' to "Ginger"!
// (send message "name:" with arument containing slot 'new' with the new value)
ginger |> send "name:" (makeString "Ginger") |> send "print" empty |> ignore

// TODO: Write code to change the book of 'ginger' to "Goodbye, Mog"!
ginger |> send "book:" (makeString "Goodbye, Mog") |> send "print" empty |> ignore

// TODO: What do we get if we run the following now?
mog |> send "name" empty |> send "print" empty |> ignore
mog |> send "book" empty |> send "print" empty |> ignore
ginger |> send "name" empty |> send "print" empty |> ignore
ginger |> send "book" empty |> send "print" empty |> ignore
