namespace Intro

module Code =
  type Pet = Dog | Cat | Snake

module Say =
    let hello name =
        printfn "Hello %s" name
