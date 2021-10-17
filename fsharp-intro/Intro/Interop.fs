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

  // Weitere Konstruktoren mit 'new' moeglich, aber selten nuetzlich
  //  new () = Dillo (5) // Default-Wert (weiter unten optionale Parameter)
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

  // -> Constraints!
  // Persoenlich selten verwendet ausser bei Interop mit Windows Forms,
  // um auf bestimmte Parent-Typen einzugrenzen
  // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/constraints

  // Anmerkung: es gibt auch C#-Extensionmethoden via [<Extension>],
  // aber selten noetig (idR nur dann, wenn man auf generischen Typen
  // mit bestimmter Auspraegung etwas definieren will).

  // Type-Extensions: Methoden ankleben (im gleichen File -> intrinsisch, sonst optional [nur bei open])
  // Manchmal praktisch, zB fuer Hilfsfunktionen
  type List<'a> with
    // Statisch, nicht-statisch, Properties (aber nicht SRTPs)
    static member cons x xs = x :: xs
  // Erst: ohne ?
  // Kurzform: optionaler Parameter

  // SRTP (nuetzlich bzw. noetig z.B. bei inline-Operationen mit Constraints):
  // Zur Kompilierzeit kann getypcheckt werden, ob bestimmte Operationen
  // erlaubt sind (erkennbar am ^a statt 'a wie bei generischen Parametern):
  // quasi Beispiel aus MSDN: Spezielle Addition (inline) auf ungleichen Typen.
  // -> Zur Kompilierzeit muss gecheckt werden, ob der Operator fuer die Kombination
  // aus Werten beim Aufruf exisiert.
  // Beispiel: Wir wollen Strings zu Integern addieren (Laenge + Wert)
  let inline add(v: ^a when (^a or ^b) : (static member (+|+): ^a * ^b -> ^a), w : ^b) =
    v +|+ w

  // Anmerkungen und Restriktionen:
  // - Member-Constraints erinnern an Typklassen aus Haskell
  // - Muessen inline definiert werden (unter der Haube wird verschiedener
  //   Code fuer die Kombinationen generiert, und der passende wird zur Kompilierzeit
  //   an der Aufrufstelle eingesetzt)
  // - Muessen SRTPs benutzen; normale Generics gehen nicht
  // - Muss kein Operator sein
  // - Oft durch Interfaces ersetzbar mit besserer Lesbarkeit der Signaturen und Kontrakte
  // - Inlining kann Performce-Boost bringen
  // - Inline wurde eingefuehrt, um arithmetische Operationen auf generischen Zahlentypen
  //   definieren zu koennen (sonst wird bei let add x y = x + y) immer ein bestimmter Typ
  //   inferiert; durch Inlining wird dies uebersetzt in Constraint, der aussagt, dass es
  //   eine bestimmte, spezifische Operation (+) geben muss, die "passt"

  // Idee: Extension-Methode auf System.String:
  //type System.String with
    //static member (+|+) (s: string, i: int) = s.Length + i
  // Aber Extension-Methoden duerfen keine solchen Operatoren definieren

  // -> Wrapper-Typ
  type MyString =
    | MyString of System.String
    static member (+|+) (MyString s, i: int) = s.Length + i
    // Beispiel in main

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

  // Abstrakte Basisklasse, von der geerbt werden kann: Attribut
  [<AbstractClass>]
  type MyBase (s: string) =
    let name = s
    abstract member Talk: unit -> unit
    // Auch implementierte Member moeglich
    member _.TalkToPeter () = printfn "Hi, Peter"

  // Delegates: ab und an hat C# (statt Action<T>, Func<S,T>, Predicate<T>) Delegates,
  // die man bedienen muss.
  type MyFunc = delegate of string -> int

  let callMyFuncWithAbc (myFunc: MyFunc) = myFunc.Invoke "abc"
  let callFWithAbc (myFuncAlt: System.Func<string, int>) = myFuncAlt.Invoke("abc")
  // Auch nett: Invoke-Aufruf binden
  let callMyFunc (myFunc: MyFunc) = myFunc.Invoke
  let callMyFuncWithAbc' myFunc = callMyFunc myFunc "abc"

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
    printfn "abcde +|+ 3 = %i" (MyString "abcde" +|+ 3)
    let result = Tup(2,3) + Tup(7,8)
    // Danach ToString
    printfn "Added tuples: %A" result
