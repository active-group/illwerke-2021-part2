namespace Intro

open NUnit.Framework
open FsUnitTyped

module DecodeTest =
  open Decode

  let shouldBeError = function
  | Error _ -> ()
  | Ok a -> failwithf "Expected Error, got Ok %A" a

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
      actual |> shouldEqual (Ok "abc foo")

  module DecodeSpecificStringTests =
    [<Test>]
    let ``works with the correct JSON string`` () =
      let input = Json.String "hi there"
      let actual = runDecoder (decodeSpecificString "hi there" 3) input
      actual |> shouldEqual (Ok 3)

    [<Test>]
    let ``fails with incorrect JSON string`` () =
      let input = Json.String "hi there"
      let actual = runDecoder (decodeSpecificString "there" 3) input
      actual |> shouldBeError

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
