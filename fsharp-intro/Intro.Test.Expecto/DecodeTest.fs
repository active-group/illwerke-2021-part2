namespace Intro

open Expecto

module DecodeTest =
  open Decode

  [<Tests>]
  let decodeStringTests =
    testList "decodeString" [
      test "fails when JSON is not a string" {
        let input = Json.Boolean true
        let actual = runDecoder decodeString input
        Expect.isError actual "decoded boolean as string"
      }

      test "correctly decodes string values" {
        let input = Json.String "abc foo"
        let actual = runDecoder decodeString input
        Expect.equal (Ok "abc foo") actual "decoded "
      }
    ]

  // analog für decodeInt, decodeBool, decodeNull, ...

  [<Tests>]
  let decodeSpecificStringTests = testList "decodeSpecificString" [
    test "works with the correct JSON string" {
      let result = runDecoder (decodeSpecificString "foobar" 3) (Json.String "foobar")
      Expect.equal result (Ok 3) "specific string failure"
    }
  ]

  [<Tests>]
  let decodeSuitTests = testList "decodeSuit" [
    test "works for diamonds" {
      let result = runDecoder DecodeCards.decodeSuit (Json.String "Karo")
      Expect.equal result (Ok Cards.Diamonds) ""
    }
  ]

  [<Tests>]
  let succeedTests =
    // succeed should always succeed -> Property
    testProperty "succeed always succeeds" (fun (json: Json.Value) value ->
      runDecoder (succeed value) json = Ok value)

  // konditionale Eigenschaften (braucht diesen Import)
  open FsCheck

  [<Tests>]
  let decodeStringTests2 =
    let isNiceString (s: string) = s <> null
    testList "properties for decodeString" [
      testProperty "succeed succeeds for nice strings" (fun (s: string) ->
        isNiceString s ==> lazy (runDecoder decodeString (Json.String s) = Ok s))
    ]

  //Gen.sample 15 Arb.generate<list<int>>

  // Arbitrary-Implementierung nötig, aber F# macht dies meistens automatisch via Reflection.
  // Würden wir das händisch machen wollen/müssen (z.B. für rekursive Datentypen),
  // dann benutzt man Generatoren (auch mit CEs):

  type Foo =
    | Bar of string * int
    | Baz of list<bool>

  open FsCheck

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
      }
    ]

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

  [<Tests>]
  let decodeListTests = testList "decodeList" [
    test "works for the empty list" {
      let result = runDecoder (decodeList decodeInt) (Json.Array [])
      Expect.equal result (Ok []) ""
    }
    test "works for two simple strings" {
      let result =
        [Json.String "a"; Json.String "b"]
        |> Json.Array
        |> runDecoder (decodeList decodeString)
      Expect.equal result (Ok ["a"; "b"]) ""
    }
  ]

  // Property-Test, der prüft, ob (im Wesentlichen) gilt:
  // Dekodieren nach Kodieren == Identität
  // (für CardsPlayed)
  [<Tests>]
  let decodeCardsPlayedTests =
    testProperty "decode after encode is id" (fun cardsPlayed ->
      let result =
        cardsPlayed
        |> Cards.encodeCardsPlayed
        |> runDecoder DecodeCards.decodeCardsPlayed
      result = Ok cardsPlayed)
