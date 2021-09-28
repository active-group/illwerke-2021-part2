namespace Intro

module Code =
  type Pet = Dog | Cat | Snake

  // Ist Haustier niedlich?
  let isCute (pet: Pet): bool =
    match pet with
    | Dog -> true
    | Cat -> true
    | Snake -> false 

module Say =
    let hello name =
        printfn "Hello %s" name
