namespace Intro

module Code =
  // Aufzählung
  type Pet = Dog | Cat | Snake

  // Ist Haustier niedlich?
  let isCute (pet: Pet): bool =
    match pet with
    | Dog -> true
    | Cat -> true
    | Snake -> false 

  let isCute' =
   fun (pet: Pet) ->
      match pet with
      | Dog -> true
      | Cat -> true
      | Snake -> false 


  // zusammengesetzte Daten
  // type Time = { Hour: int; Minute: int }
  type Time = { Hour: int 
                Minute: int }

  // Mehrdeutigkeit bzgl. Hour und Minute
  // type PreciseTime = { Hour: int; Minute: int; Seconds: int }

  let time1 = { Hour = 12; Minute = 24 }
  let time2 = { Hour = 5; Minute = 37}

  // Minuten seit Mitternacht
  let msm (time: Time): int =
    time.Hour * 60 + time.Minute

  // Variante
  let msm' ({ Hour = h; Minute = m}): int =
    h * 60 + m

  type Liveness = Dead | Alive

  type Weight = double

  // Ein Tier ist eins der folgenden:
  // - Gürteltier
  // - Papagei
  // algebraischer Datentyp
  // discriminated union
  type Animal =
    | Dillo of Liveness * Weight
    | Parrot of string * Weight

  // Gürteltier, lebendig, 10kg
  let dillo1 = Dillo (Alive, 10.0)
  // totes Gürteltier, 9kg
  let dillo2 = Dillo (Dead, 9.0)

  let parrot1 = Parrot ("Hello!", 1.0)
  let parrot2 = Parrot ("Goodbye!", 1.5)
  
  // Gürteltier überfahren
  let runOverAnimal (animal: Animal): Animal =
    match animal with
    | Dillo (_, w) -> Dillo (Dead, w)
    | Parrot (_, weight) -> Parrot ("", weight)

  // Weight -> (Animal -> Animal)
  let feedAnimal (animal: Animal) (amount: Weight): Animal =
    match animal with
//    | Dillo (liveness, weight) ->
//        match liveness with
//        | Dead -> animal
//        | Alive -> Dillo (liveness, weight + amount)
    | Dillo (Alive, weight) -> Dillo (Alive, weight + amount)
    | Dillo (Dead, _) -> animal
    | Parrot (sentence, weight) -> 
        Parrot (sentence, weight + amount)

  // Langschreibweise von feedAnimal
  let feedAnimal'' =
    fun (animal: Animal) ->
      fun (amount: Weight) ->
        match animal with
        | Dillo (Alive, weight) -> Dillo (Alive, weight + amount)
        | Dillo (Dead, _) -> animal
        | Parrot (sentence, weight) -> 
            Parrot (sentence, weight + amount)


  // ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
  let flip f b a = f a b

  // fun = lambda
  let flip' (f: 'a -> 'b -> 'c): ('b -> ('a -> 'c)) =
    fun b -> fun a -> f a b


  let feedAnimal' (amount: Weight, animal: Animal): Animal =
    match animal with
    | Dillo (Alive, weight) -> Dillo (Alive, weight + amount)
    | Dillo (Dead, _) -> animal
    | Parrot (sentence, weight) -> 
        Parrot (sentence, weight + amount)

  // Eine geometrische Figur ("shape") ist eins der folgenden:
  // - ein Kreis
  // - ein Quadrat
  // - eine Überlagerung zweier geometrischer Figuren
  // 1. Datenanalyse
  // 2. Funktion: Ist ein Punkt innerhalb einer geometrischen Figur?  (oder außerhalb)


module Say =
    let hello name =
        printfn "Hello %s" name
