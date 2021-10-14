namespace Intro

module Cards =
  type Suit = Diamonds | Hearts | Spades | Clubs
  
  type Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  
  // Eine Spielkarte besteht aus einer Farbe und einem Wert
  type Card =
    { Rank : Rank
      Suit : Suit
    }

  // Eine Liste von (gespielten) Karten
  type CardsPlayed = list<Card>
  //type CardsPlayed = list<Card>
  // nach decodeList
  //type CardsPlayed = CardsPlayed of list<Card>

  // String-Repraesentation einer Farbe
  let suitToString = function
    //| Diamonds -> nameof Diamonds
    | Diamonds -> "Karo"
    | Hearts -> "Herz"
    | Spades -> "Pik"
    | Clubs -> "Kreuz"

  // Serialisiere Farbe -> JSON
  let encodeSuit = Json.String << suitToString

  // String-Repraesentation eines Kartenwert
  let rankToString = function
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "Bube"
    | Queen -> "Dame"
    | King -> "König"
    | Ace -> "Ass"

  // Serialisiere Wert -> JSON
  let encodeRank = Json.String << rankToString

  // FIXME: Kann man das Bauen von Objekten und Listen auch schoen mit CEs machen?
  let encodeCard { Rank = rank; Suit = suit } =
    Map.ofList [
        "rank", encodeRank rank
        "suit", encodeSuit suit
    ]
    |> Json.Object

  let encodeCardsPlayed =
    //cardsPlayed
    //|> List.map encodeCard
    //|> Json.Array
    Json.Array << List.map encodeCard
