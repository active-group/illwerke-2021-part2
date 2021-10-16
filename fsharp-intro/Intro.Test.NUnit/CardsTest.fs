namespace Intro

open NUnit.Framework
open FsUnitTyped

module CardsTest =
  open Cards

  // Für Gliederung eventuell:
  //[<TestFixture>]
  //type SuitToStringTest () =
  //
  // oder:
  // module SuitToStringTest =

  //[<Test>]
  //let ``Diamonds is serialized correctly`` () =
  //  let actual = suitToString Diamonds
  //  "Karo" |> shouldEqual actual

  //[<Test>]
  //let ``Hearts is serialized correctly`` () =
  //  let actual = suitToString Hearts
  //  "Herz" |> shouldEqual actual

  //[<Test>]
  //let ``Spades is serialized correctly`` () =
  //  let actual = suitToString Spades
  //  "Pik" |> shouldEqual actual

  //[<Test>]
  //let ``Clubs is serialized correctly`` () =
  //  let actual = suitToString Clubs
  //  "Kreuz" |> shouldEqual actual

  let suitToStringCases = [
    TestCaseData(Clubs, "Kreuz")
    TestCaseData(Spades, "Pik")
    TestCaseData(Hearts, "Herz")
    TestCaseData(Diamonds, "Karo")
  ]

  [<TestCaseSource(nameof suitToStringCases)>]
  let ``suitToString works`` (suit, str) =
    suitToString suit |> shouldEqual str

  let rankToStringCases = [
    TestCaseData(Seven, "7")
    TestCaseData(Eight, "8")
    TestCaseData(Nine, "9")
    TestCaseData(Ten, "10")
    TestCaseData(Jack, "Bube")
    TestCaseData(Queen, "Dame")
    TestCaseData(King, "König")
    TestCaseData(Ace, "Ass")
  ]

  [<TestCaseSource(nameof rankToStringCases)>]
  let ``rankToString works`` (rank, str) =
    rankToString rank |> shouldEqual str

  // Hier können wir nicht mehr gut alle Kombinationen hinschreiben
  [<Test>]
  let ``An example card is encoded correctly`` () =
    let encodedCard = { Rank = Queen; Suit = Clubs } |> encodeCard
    let expected =
      Map.ofList [
        "rank", Json.String "Dame"
        "suit", Json.String "Kreuz"]
      |> Json.Object
    expected |> shouldEqual encodedCard
