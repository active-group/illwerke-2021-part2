namespace Intro

module Decode =

  type DecodeError =
    // Dekodieren ist fehlgeschlagen
    | Failure of string * Json.Value
    | Field of string * DecodeError
    | OneOf of list<DecodeError>

  // type Either<'a, 'b> = Left of 'a | Right of 'b
  // F# hat Result

  // Ein Dekodierer dekodiert zu einem Wert vom Typ 'a.
  // Dies kann fehlschlagen, daher verwenden wir Either.
  type Decoder<'a> = Decoder of (Json.Value -> Result<'a, DecodeError>)

  let runDecoder (Decoder decode) json =
    decode json

  let decodeString: Decoder<string> = Decoder (fun json ->
    match json with
    | Json.String s -> Ok s
    | _ -> Failure ("Not a string", json) |> Error)

  let decodeInt: Decoder<int> = Decoder (fun json ->
    match json with
    | Json.Number i -> Ok i // Achtung! Hier muesste normalerweise ein Check hin, ob i "wirklich" int ist
    | _ -> Failure ("Not an integer", json) |> Error)

  let decodeBool: Decoder<bool> = Decoder (fun json ->
    match json with
    | Json.Boolean b -> Ok b
    | _ -> Failure ("Not a Boolean", json) |> Error)

  // Kombinatoren: "Higher-Order-Dekodierer":
  // Baue Dekodierer aus anderen Dekodierern/Bausteinen

  // DecodeCards: hier decodeRank und decodeSuit schreiben

  // Der erste wirkliche Kombinator: gegeben ein Wert resultVal,
  // liefere einen Dekodierer, der bei Null den Wert zurueckgibt
  let decodeNull (resultVal: 'a): Decoder<'a> = Decoder (fun json ->
    match json with
    | Json.Null -> Ok resultVal
    | _ -> Failure ("Not null", json) |> Error)

  // Decoder, der stets das gegebene x dekodiert und den Input ignoriert.
  // Vergleiche Implementierung von _.Return in Maybe!
  let succeed (x: 'a): Decoder<'a> = Decoder (fun _ -> Ok x)

  // Decoder, der immer fehlschlaegt
  let fail (msg: string): Decoder<'a> =
    Decoder (function | json -> Failure (msg, json) |> Error)

  let decodeField (field: string) aDecoder: Decoder<'a> =
    Decoder (fun json ->
        match json with
        | Json.Object m ->
          match Map.tryFind field m with
          | None -> Failure (sprintf "Field %s not found" field, json) |> Error
          | Some innerJson ->
            runDecoder aDecoder innerJson
            // Falls das fehlschlaegt, wollen wir eine bessere Fehlermeldung zurueckgeben,
            // auch um sie vom aeusseren Fehler unterscheiden zu koennen -> Tag mit Fieldname
            |> Result.mapError (fun e -> Field (field, e))
        | _ -> Failure ("Not a JSON object", json) |> Error)

  // DecodeCards: hier decodeCard schreiben

  // Exkurs Applicative mit Ziel, decodeList zu schreiben fuer CardsPlayed

  // Wir koennen ueber einen Decoder mappen und damit den Rueckgabewert aendern.
  // Bzw.: Wir liften eine Funktion 'a -> 'b "in" Decoder:
  //   Decoder<'a> -> Decoder<'b>
  let map (f: 'a -> 'b) (decoder: Decoder<'a>) = Decoder (fun json ->
    match runDecoder decoder json with
    | Ok res -> Ok (f res)
    | Error err -> Error err)

  // map : 'a -> 'b -> Decoder<'a> -> Decoder<'b>
  // wollen: map2 : 'a -> 'b -> 'c -> Decoder<'a> -> Decoder<'b> -> Decoder<'c>
  // apply : Decoder<'a -> 'b> -> Decoder<'a> -> Decoder<'b>
  //               ^ Wo taucht so etwas "natuerlich" auf?

  // NACH map2, map3, ...!
  let apply (aToBDecoder: Decoder<'a -> 'b>) aDecoder =
    Decoder (fun json ->
      match runDecoder aDecoder json with
      | Error e -> Error e
      | Ok a ->
        match runDecoder aToBDecoder json with
        | Error e -> Error e
        | Ok f -> f a |> Ok)

  let map2 (f: 'a -> 'b -> 'c) (aDecoder: Decoder<'a>) (bDecoder: Decoder<'b>) =
    // f : 'a -> ('b -> 'c)
    // map f : Decoder<'a> -> Decoder<'b -> 'c>
    // map f aDecoder : Decoder<'b -> 'c>
    apply (map f aDecoder) bDecoder

  // Was ist mit map3, map4 usw.?

  let (<*>) = apply

  let map3 f aDec bDec cDec =
    map f aDec <*> bDec <*> cDec

  let map4 f aDec bDec cDec dDec =
    map f aDec <*> bDec <*> cDec <*> dDec

  // Auch hier koennen wir eine Bind-Operation definieren, die einen
  // Decoder nimmt und mit einer Funktion verkettet, die auf dem Ergebnis
  // operiert und einen weiteren Decoder liefert:
  let bind: Decoder<'a> -> ('a -> Decoder<'b>) -> Decoder<'b> =
    fun (Decoder decodeA) f ->
      Decoder (fun json ->
        match decodeA json with
        | Ok a ->
          // a ist das 'a im Erfolgsfall
          let (Decoder decodeB) = f a
          decodeB json
        | Error e -> Error e)

  let applyResult (aToBResult: Result<'a -> 'b, 'error>) aResult =
    //match aResult with
    //| Error e -> Error e
    //| Ok a ->
    //    match aToBResult with
    //    | Error e -> Error e
    //    | Ok f -> Ok (f a)
    match aToBResult with
    | Ok f -> Result.map f aResult
    | Error e -> Error e

  let map2Result: ('a -> 'b -> 'c) -> Result<'a, 'error> -> Result<'b, 'error> -> Result<'c, 'error> =
    fun f aResult bResult ->
      applyResult (Result.map f aResult) bResult

  let traverseResult: ('a -> Result<'b, 'error>) -> list<'a> -> Result<list<'b>, 'error> =
    let cons x rest = x :: rest
    fun f xs ->
      List.foldBack
        (fun a acc -> map2Result cons (f a) acc)
        xs
        (Ok [])

  // Dekodiere eine Liste von 'as anhand eines Dekodierers fuer 'a
  let decodeList (Decoder decodeItem): Decoder<list<'a>> =
    Decoder (function
      | Json.Array arr ->
        // Wollen decodeItem ueber die Liste mappen. ABER: falscher Typ!
        arr |> traverseResult decodeItem
      | json -> Failure ("Not a JSON array", json) |> Error)

  let oks (results: list<Result<'a, 'error>>) =
    results
    |> List.collect (function | Ok a -> [a] | Error _ -> [])

  let errors (results: list<Result<'a, 'error>>) =
    results
    |> List.collect (function | Error e -> [e] | Ok _ -> [])

  // "Einer wird schon passen"
  // Laziness waere nett hier
  let decodeOneOf (aDecoders: list<Decoder<'a>>) = Decoder (fun json ->
    let results = aDecoders |> List.map (fun decoder -> runDecoder decoder json)
    match oks results with
    | [] -> OneOf (errors results) |> Error
    | x::_ -> Ok x)

  let decodeOptional (aDecoder: Decoder<'a>): Decoder<option<'a>> =
    decodeOneOf [map Some aDecoder; succeed None]

  type DecoderBuilder () =
    // Quasi map
    member _.BindReturn (aDecoder, f) = map f aDecoder
    //member _.ReturnFrom x = x
    // benoetigt fuer and!
    // Idee: fuehre beide Dekodierer "parallel" aus, sie sind gleichwertig,
    // dann fasse Ergebnisse in Tupel zusammen
    member _.MergeSources (aDecoder, bDecoder) =
      //Decoder (fun json ->
      //    let aRes = runDecoder aDecoder json
      //    let bRes = runDecoder bDecoder json
      //    match aRes, bRes with
      //    | Ok a, Ok b -> Ok (a, b)
      //    | Error e, Ok _ -> Error e
      //    | Ok _, Error e -> Error e
      //    | Error e1, Error e2 -> OneOf [e1; e2] |> Error)
      // besser:
      let toTuple a b = a, b
      map2 toTuple aDecoder bDecoder
    // Nur benötigt für decodeSpecificString
    // let! monadisch (mehrere let!-Aufrufe nach and!, oder ohne and!)
    member _.Bind (aDecoder, binder) = bind aDecoder binder
    // return
    member _.Return a = succeed a
    // return!
    member _.ReturnFrom aDecoder = aDecoder

  let decoder = DecoderBuilder ()

  let decodeSpecificString (s: string) (result: 'a) =
    decoder {
      let! str = decodeString
      if str = s
        then return result
        else return! fail (sprintf "Expected %s" s)
    }

module DecodeCards =
  open Cards
  open Decode

  let decodeSuit: Decoder<Cards.Suit> =
    decodeOneOf [
      decodeSpecificString "Karo" Cards.Diamonds
      decodeSpecificString "Kreuz" Cards.Clubs
      decodeSpecificString "Pik" Cards.Spades
      decodeSpecificString "Herz" Cards.Hearts
    ] 

  // Alternativvorgehen:
  let rankFromString: string -> Cards.Rank = function
    | "7" -> Cards.Seven
    | "8" -> Cards.Eight
    | "9" -> Cards.Nine
    | "10" -> Cards.Ten
    | "Bube" -> Cards.Jack
    | "Dame" -> Cards.Queen
    | "König" -> Cards.King
    | "Ass" -> Cards.Ace
    // nicht schoen
    | s -> failwithf "%s is not a valid value for Rank" s

  let decodeRank: Decoder<Cards.Rank> =
    map rankFromString decodeString

  let decodeCard: Decoder<Cards.Card> =
    decoder {
      let! suit = decodeField "suit" decodeSuit
      and! rank = decodeField "rank" decodeRank
      return { Rank = rank; Suit = suit }
    }

  let decodeCardsPlayed: Decoder<Cards.CardsPlayed> =
    decodeList decodeCard

  // Mehrstellige Funktionen: applikatives Anwenden bzw. mapN
  type CardRankCardsPlayed = CardRankCardsPlayed of Cards.Card * Cards.Rank * Cards.CardsPlayed

  let curry3 f a b c = f (a, b, c)

  let decodeCardRankCardsPlayed: Decoder<CardRankCardsPlayed> =
    //map3 (curry3 CardRankCardsPlayed) decodeCard decodeRank decodeCardsPlayed
    //decoder {
    //    let! card = decodeCard
    //    let! rank = decodeRank
    //    let! cardsPlayed = decodeCardsPlayed
    //    return CardRankCardsPlayed (card, rank, cardsPlayed)
    //}
    // Alternativ:
    decoder {
      let! card = decodeCard
      // and! macht den applikativen Gedanken klar: die drei decodeX-Aufrufe
      // sind unabhaengig voneinander
      and! rank = decodeRank
      and! cardsPlayed = decodeCardsPlayed
      return CardRankCardsPlayed (card, rank, cardsPlayed)
    }
