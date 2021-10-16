namespace Intro

open NUnit.Framework
open FsUnitTyped

module DecodeTest =
  open Decode

  let shouldBeError = function
  | Error _ -> ()
  | Ok a -> failwithf "Expected Error, got Ok %A" a

  // Später evtl.
  let shouldBeOk a = function
  | Error err -> failwithf "Got Error %A instead of Ok" err
  // Anstatt händisch hier zu vergleichen: Bibliotheksfunktion für Diffing
  | Ok x -> shouldEqual a x

  // Alternative zu Klasse: Untermodule -> schönere Gliederung in Test Explorer
  [<TestFixture>]
  type DecodeStringTests () =
    [<Test>]
    member _.``fails when JSON is not a string`` () =
      let input = Json.Boolean false
      let actual = runDecoder decodeString input
      actual |> shouldBeError

    [<Test>]
    member _.``correctly decodes string values`` () =
      let input = Json.String "abc foo"
      let actual = runDecoder decodeString input
      actual |> shouldBeOk "abc foo"

  module DecodeSpecificStringTests =
    [<Test>]
    let ``works with the correct JSON string`` () =
      let input = Json.String "hi there"
      let actual = runDecoder (decodeSpecificString "hi there" 3) input
      actual |> shouldBeOk 3

    [<Test>]
    let ``fails with incorrect JSON string`` () =
      let input = Json.String "hi there"
      let actual = runDecoder (decodeSpecificString "there" 3) input
      actual |> shouldBeError

  open FsCheck.NUnit

  module SucceedTests =
    [<Test>]
    let ``succeed succeeds?`` () =
      let input = Json.String "abc"
      let actual = runDecoder (succeed 5) input
      actual |> shouldBeOk 5
    // - Prüft nur wenig Input (sollte für alle JSON-Werte gelten)
    // - Prüft nur wenig Output (sollte nicht nur für Integer gelten)
    // --> PBT gibt uns die Möglichkeit, das auszudrücken
    //     - Property kann betrachtet werden als Funktion von getypten Werten -> bool
    //     - Erstellt zufällig Werte
    //     - Bei Misserfolg (false): shrinking: das Ergebnis wird vereinfacht

    [<Property>]
    //                                                    v eventuell nicht, was wir wollen
    let ``succeed always succeeds`` (json: Json.Value, x: obj) =
      runDecoder (succeed x) json = Ok x

  // Geht auch z.B. für decodeString, aber: wollen kein NULL!
  // Conditional properties:
  let isNiceString (s: string) = s <> null

  open FsCheck

  [<Property>]
  let ``decodeString succeeds for nice strings`` (s: string) =
    // Hinweisen: manchmal ist lazy nötig:
    // bspw. a <> 0 ==> 1/a = 1/a
    //s <> null ==> lazy (runDecoder decodeString (Json.String s) = Ok s)
    s <> null ==> (runDecoder decodeString (Json.String s) = Ok s)

  //Gen.sample 15 Arb.generate<list<int>>

  // Arbitrary-Implementierung nötig, aber F# macht dies meistens automatisch via Reflection.
  // Würden wir das händisch machen wollen/müssen (z.B. für rekursive Datentypen),
  // dann benutzt man Generatoren (auch mit CEs):

  type Foo =
    | Bar of string * int
    | Baz of list<bool>

  let fooGen =
    // Beide Konstruktoren gleich häufig
    Gen.oneof [
      gen {
        let! s = Arb.generate // Arb.generate<string>
        let! i = Arb.generate
        return Bar (s, i)
      }
      gen {
        let! bools = Gen.listOf Arb.generate<bool>
        return Baz bools
      }]

  // Beispiel: Häufigkeit der Konstruktoren steuern
  let fooGenWithFreqs =
   // Kürzere Schreibweise dank Kombinatoren wie bei Decode
   Gen.frequency [
     1, Gen.map2 (fun s i -> Bar (s, i)) Arb.generate Arb.generate
     17, Gen.map Baz Arb.generate
   ]

  // Rekursive Typen sind problematisch, da hier Potenzial für
  // StackOverflow besteht -> Size-Mechanismus manuell bedienen, um bei
  // höherer Tiefe weniger zu generieren:

  type MyRec = Bottom | Node of int * MyRec

  let myRecGen =
    // Higher-Order-Generator
    let rec myRecSized size =
      match size with
      | 0 -> Gen.constant Bottom
      | n when n > 0 ->
        // smallerMyRec ist wieder ein Generator, aber "kleiner"
        let smallerMyRec = myRecSized (size / 2)
        Gen.oneof [ Gen.constant Bottom
                    Gen.map2 (fun i mr -> Node (i, mr)) Arb.generate Arb.generate
        ]
      | _ -> failwith "Nur positive sizes erlaubt"
    Gen.sized myRecSized

   // Aber! F# muss diese selbstgeschriebenen Generatoren erst kennenlernen:

  type MyGenerators () =
    static member Foo () =
      //{ new Arbitrary<Foo> () with
      //    override _.Generator = fooGenWithFreqs
      //    override _.Shrinker t = Seq.empty }
      Arb.fromGen fooGenWithFreqs

    static member MyRec () =
      Arb.fromGen myRecGen

  // Registriere die Generatoren global. FsCheck schaut in der Liste der
  // Registrierungen nach, ob etwas existiert oder ob Reflection benutzt werden muss.
  // Liste: Arb.Default
  // Zu bestimmtem Typ "nachschauen": Arb.from<'a>
  Arb.register<MyGenerators> () |> ignore

  module DecodeListTest = 
    [<Property>]
    let ``decodeList works for the empty list`` () =
      let actual = Json.Array [] |> runDecoder (decodeList decodeInt)
      actual |> shouldBeOk []

  //module DecodeFieldTests =
  //  ...

module DecodeCardsTest =
  open Cards
  open Decode
  open DecodeCards

  module DecodeSuitTests =
    [<Test>]
    let ``decodes an example suit correctly`` () =
      let input = Json.String "Karo"
      let actual = runDecoder decodeSuit input
      actual |> shouldEqual (Ok Diamonds)
