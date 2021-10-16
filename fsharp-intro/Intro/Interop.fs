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

  // Auch Folgendes ist möglich:
  type MyType =
    | Foo
    | Bar
    // Member "drankleben"
    member _.SayHi text = printfn "%s" text
    // Standard-.NET-Member überschreiben
    override _.ToString () = "MyType, always"

  // Interfaces definieren Schnittstellen, aber haben keine
  // Default-Implementierung (Hinweis: geht aber mittlerweile auch)

  type IAnimal =
    abstract member Alive: bool
    abstract member RunOver: unit -> unit

  //type Dillo (weight: int) =
  //  let mutable alive = true

  //  member _.Weight = weight

  //  interface IAnimal with
  //    member _.Alive = alive
  //    member _.RunOver () = alive <- false

  // Wollen Dillo mit anderen Dillos vom Gewicht her
  // vergleichen können.
  type Dillo<'a when 'a : comparison> (weight: 'a) =
    let mutable alive = true

    member _.Weight = weight
    member x.IsHeavierThan (other: Dillo<'a>) =
      x.Weight > other.Weight

    interface IAnimal with
      member _.Alive = alive
      member _.RunOver () = alive <- false

  // Erst: ohne ?
  // Kurzform: optionaler Parameter
  type Snake (?thickness: int) =
  //type Snake (thickness: int) =
    //let mutable thickness = thickness
    // Die durchschnittliche Schlange ist 5 dick -> Measures erwähnen
    let mutable thickness = Option.defaultValue 5 thickness

    member _.Thickness = thickness
    
    interface IAnimal with
      member _.Alive = thickness > 0
      member x.RunOver () = thickness <- 0

  // Non Konstruktoraufruf ohne Argument möglich
  let defaultSnake = Snake ()

  // In F# kann man Interfaces auch ohne Klasse ad-hoc implementieren:
  // Object Expressions
  let createImmortalAnimal (): IAnimal =
    { new IAnimal with
        member _.Alive = true
        member _.RunOver () = ()
    }

  // Interfaces können von anderen Interfaces ableiten
  type IFlyingAnimal =
    // Inherit wird analoag auch für Klassenvererbung genutzt
    inherit IAnimal
    abstract member Fly: unit -> unit

  // Es reicht, das spezifischere Interface explizit zu implementieren
  type Duck () =
    let mutable alive = true
    interface IFlyingAnimal with
      member _.Alive = alive
      member x.RunOver () = alive <- false
      member x.Fly () = printfn "I'm flying!"

  // Operatoren überladen: wollen Tupel addieren können, wenn Inhalt punktweise addiert werden kann
  // Inferierter Typ: ints
  //let addTups (a, b) (c, d) = (a + c, b + d)
  type Tup (x, y) =
    member _.First = x
    member _.Second = y
    override _.ToString () = sprintf "( %A, %A )" x y
    static member (+) (tup1: Tup, tup2: Tup) =
      Tup (tup1.First + tup2.First, tup1.First + tup2.Second)

  // Wenn man das tippt, sieht man, wie die Typinferenz für addTups greift
  //addTups ("", 3) (4, "c")

  let main () =
    // let foo = new MyClass ()
    let foo = MyClass ()
    foo.MyMethod 3
    foo.PrintAnotherField ()
    let dillo = Dillo 5
    printfn "%A" (dillo :> IAnimal).Alive
    // upcast ist notwendig in F# (in C#: dog.MakeSound())
    (dillo :> IAnimal).RunOver ()
    printfn "%A" (dillo :> IAnimal).Alive
    let snake = Snake 3
    printfn "%A" (snake :> IAnimal).Alive
    (snake :> IAnimal).RunOver ()
    printfn "%A" (snake :> IAnimal).Alive
    printfn "%A" snake.Thickness
    printfn "%A" ((Dillo 14).IsHeavierThan (Dillo 22))
    let immortal = createImmortalAnimal ()
    printfn "%b" immortal.Alive
    immortal.RunOver ()
    printfn "%b" immortal.Alive
    let result = Tup(2,3) + Tup(7,8)
    // Danach ToString
    printfn "Added tuples: %A" result
