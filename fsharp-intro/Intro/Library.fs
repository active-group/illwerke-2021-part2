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
                
module Say =
    let hello name =
        printfn "Hello %s" name
