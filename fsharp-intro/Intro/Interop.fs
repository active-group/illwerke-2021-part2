namespace Intro

// TODO:
// - statische Methoden
// - Vererbung? Abstrakte Basis?

module Interop =
  // .NET-Klassen in F# definieren

  //           v Konstruktor (implizit)
  type MyClass () =
    // "private" Variable, nicht mutierbar
    let someField = "abc"

    // privat und mutierbar (Vorsicht State!)
    let mutable anotherField = "bar"

    // Methoden; von außen sichtbar
    // "_" ist "this"
    member _.MyMethod (x: int) =
      // "<-" mutiert, nicht mit "=" verwechseln, sonst bool!
      anotherField <- string x + someField

    member _.PrintAnotherField () =
      printfn "Der Wert von 'anotherField' ist %s" anotherField

  // Interfaces definieren Schnittstellen, aber haben keine
  // Default-Implementierung (eigentlich neuerdings schon in C#,
  // aber mir ist nicht ganz klar, wieso. FIXME Mike?).

  // Unterschied zu Klassen: kein Konstruktor ( kein () )
  type IMakeSound =
    abstract member MakeSound: unit -> unit

  type Dog () =
    interface IMakeSound with
      member _.MakeSound () =
        printfn "*bark*"

  // Interface-Vererbung
  type IScream =
    inherit IMakeSound
    abstract member Scream: unit -> unit

  // Ein Interface implementieren
  type Human () =
    // interface IMakeSound with
    //     member _.MakeSound () =
    //         printfn "I can speak"
    interface IScream with
      // Das alleine reicht nicht aus: müssen auch IMakeSound implementieren.
      // Das geht aber im gleichen "with"
      member _.Scream () =
        printfn "WUAHHHH"
      member _.MakeSound () =
        printfn "I can speak!"

  // In F# kann man Interfaces auch ohne Klasse implementieren:
  // Object Expressions
  let createSoundMaker (s: string): IMakeSound =
    { new IMakeSound with
          member _.MakeSound () =
              printfn "%s" s
    }

  // Generics können manchmal nützlich sein, z.B., um dünne F#-API
  // auf C#-API zu setzen

  // TODO Besseres Beispiel

  type IMyContainer<'a> =
    abstract member Items: 'a list
    abstract member Add: 'a -> unit

  open System.Collections.Generic

  type MyContainer<'a> (initialItems: list<'a>) =
    let items: IEnumerable<'a> = seq initialItems
    member _.Items = items |> Seq.toList
    member _.AddItem item = () // TODO

  let main () =
    // let foo = new MyClass ()
    let foo = MyClass ()
    foo.MyMethod 3
    foo.PrintAnotherField ()
    let dog = Dog ()
    let human = Human ()
    // upcast ist notwendig in F# (in C#: dog.MakeSound())
    (dog :> IMakeSound).MakeSound ()
    // Beide Aufrufe möglich mit IScream
    (human :> IScream).MakeSound ()
    (human :> IScream).Scream ()
    (createSoundMaker "hello there").MakeSound ()

  // TODO Klassen mit Generic?
