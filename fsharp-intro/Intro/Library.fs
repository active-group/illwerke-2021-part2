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

  // zusammengesetzte Daten
  // type Time = { Hour: int; Minute: int }
  type Time = { Hour: int 
                Minute: int }

  let time1 = { Hour = 12; Minute = 24 }
  let time2 = { Hour = 5; Minute = 37}

module Say =
    let hello name =
        printfn "Hello %s" name
