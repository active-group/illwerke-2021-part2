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

  // TODO Besseres Beispiel

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

  type Snake (thickness: int) =
    let mutable thickness = thickness

    member _.Thickness = thickness
    
    interface IAnimal with
      member _.Alive = thickness > 0
      member x.RunOver () = thickness <- 0

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
    let dillo = Dillo 5
    printfn "%A" (dillo :> IAnimal).Alive
    (dillo :> IAnimal).RunOver ()
    printfn "%A" (dillo :> IAnimal).Alive
    let snake = Snake 3
    printfn "%A" (snake :> IAnimal).Alive
    (snake :> IAnimal).RunOver ()
    printfn "%A" (snake :> IAnimal).Alive
    printfn "%A" snake.Thickness
    printfn "%A" ((Dillo 14).IsHeavierThan (Dillo 22))
    (createSoundMaker "hello there").MakeSound ()

  // TODO Klassen mit Generic?
