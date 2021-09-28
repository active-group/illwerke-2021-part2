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

  // Mehrdeutigkeit bzgl. Hour und Minute
  // type PreciseTime = { Hour: int; Minute: int; Seconds: int }

  let time1 = { Hour = 12; Minute = 24 }
  let time2 = { Hour = 5; Minute = 37}

  // Minuten seit Mitternacht
  let msm (time: Time): int =
    time.Hour * 60 + time.Minute

  // Variante
  let msm' ({ Hour = h; })

module Say =
    let hello name =
        printfn "Hello %s" name
