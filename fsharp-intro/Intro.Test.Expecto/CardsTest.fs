namespace Intro

open Expecto

module CardsTest =
  open Cards

  let checkSerializedValue (serialize: 'a -> string) (input: 'a) expected =
    test (sprintf "serialization of %A" input) {
      let actual = serialize input
      Expect.equal actual expected (sprintf "wrong serialization of %A" input)
    }

  [<Tests>]
  let suitToStringTest =
    //let check suit expected = test expected {
    //    let actual = suitToString suit
    //    Expect.equal actual expected (sprintf "wrong serialization of %A" suit)
    //}
    let check = checkSerializedValue suitToString
    testList "suitToString" [
      // Etwas umständlich: -> Hilfsfunktion
      //test "Karo" {
      //    let actual = suitToString Diamonds
      //    Expect.equal actual "Karo" "wrong serialization of Diamonds"
      //}

      //test "Kreuz" {
      //    let actual = suitToString Clubs
      //    Expect.equal actual "Kreuz" "wrong serialization of Clubs"
      //}
  
      //test "Pik" {
      //    let actual = suitToString Spades
      //    Expect.equal actual "Pik" "wrong serialization of Spades"
      //}

      //test "Herz" {
      //    let actual = suitToString Hearts
      //    Expect.equal actual "Herz" "wrong serialization of Hearts"
      //}
      check Diamonds "Karo"
      check Hearts "Herz"
      check Clubs "Kreuz"
      check Spades "Pik"
    ]

  [<Tests>]
  let rankToStringTest =
    // Brauchen gleiche Funktion wie `check`, aber für Rank -> Abstraktion
    let check = checkSerializedValue rankToString
    testList "rankToString" [
      check Seven "7"
      check Eight "8"
      check Nine "9"
      check Ten "10"
      check Jack "Bube"
      check Queen "Dame"
      check King "König"
      check Ace "Ass"
    ]
