// ----------------------------------------------------------------------------
// 01 - Implementing slot lookup
// ----------------------------------------------------------------------------

// Slots of an object have name and contents. Some can be
// marked as parent slots (message lookup looks into them).
// In Self, parent slots have * at the end of their name.
// This type is immutable. When we want to change the value,
// we replace it in the 'slots' table of the containing object.
type Slot =
  { Name : string
    Contents : Objekt
    IsParent : bool }

// Object mainly has slots. It can be runnable (and have code).
// Slots of an object with code can be used to pass arguments to it.
// In TinySelf, we also have special objects (strings, native code).
and Objekt =
  { mutable Slots : Slot list
    mutable Code : Objekt option
    mutable Special : Special option }

// Special objects are used to represent primitive string values
// and "built-in" methods not implemented in TinySelf.
and Special =
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

// Object with a list of slots and some code
let makeObject slots code =
  { Code = Some code; Special = None; Slots = slots }
// Data object is an object that does not have code
let makeDataObject slots =
  { Code = None; Special = None; Slots = slots }
// Special object such as String or Native method
let makeSpecialObject slots special =
  { Code = None; Special = Some special; Slots = slots }

// Regular (non-parent) slot with a name and an object
let makeSlot n contents =
  { Name = n; Contents = contents; IsParent = false }
// Parent slot (by convention the name should end with *)
let makeParentSlot n contents =
  { Name = n; Contents = contents; IsParent = true }

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

// See also §3.3.8 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
// Note that we do not need to keep track of visited objects as we will not
// create cyclic inheritance graphs in TinySelf.

let rec lookup (msg:string) (obj:Objekt) : list<Slot> =
  // TODO: Implement message lookup (as documented in the Self handbook)
  // * If there is a slot named 'msg' in 'obj', return that
  // * Otherwise, return all slots named 'msg' slots in objects
  //   contained in all the parent slots of 'obj' (concatenate them)
  let msgSlots = obj.Slots |> List.filter (fun s -> s.Name = msg)
  if not msgSlots.IsEmpty then
    msgSlots
  else
    match obj.Slots |> List.filter (fun s -> s.IsParent) with
    | [] -> []
    | ps -> ps |> List.map (fun s -> lookup msg s.Contents) |> List.concat


// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

// We represent strings as objects with a slot 'value' containing the special
// string object. This extra wrapping makes it easier to add methods to strings
// later, because the methods can get the value by sending 'value' to self.
let makeString s =
  makeDataObject [ makeSlot "value" (makeSpecialObject [] (String s)) ]

// Finds the slot named 'n' in the object 'o' and returs its contents
let lookupSlotValue n o =
  match lookup n o with
  | [ { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

// Get the actual string value from a string object (or fail)
let getStringValue o =
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// Ad-hoc helper for testing that prints a string result of 'lookup'
let printStringSlot slots =
  match slots with
  | [{Contents = s}] -> printfn "%s" (getStringValue s)
  | [] -> printfn "printStringSlot: Error - no slot found"
  | _ -> printfn "printStringSlot: Error - more than one slot found"

// ----------------------------------------------------------------------------
// Tests - lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let wonderland = makeDataObject [
  makeSlot "book" (makeString "Alice in Wonderland")
]

let larry = makeDataObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]
// Larry has name & sound, but no book!
larry |> lookup "name" |> printStringSlot
larry |> lookup "sound" |> printStringSlot
larry |> lookup "book" |> printStringSlot

// TODO: Cheshire cat has a name ("Cheshire Cat") and is
// both a cat (with parent 'cat') and fictional character
// from a book (with parent 'wonderland')
let cheshire = makeDataObject [
  makeParentSlot "parent*" cat
  makeParentSlot "book*" wonderland
  makeSlot "name" (makeString "Cheshire Cat")
]

// All of these should be OK!
cheshire |> lookup "name" |> printStringSlot
cheshire |> lookup "sound" |> printStringSlot
cheshire |> lookup "book" |> printStringSlot

