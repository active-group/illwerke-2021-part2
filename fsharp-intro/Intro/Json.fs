namespace Intro

module Json =
  type Value =
    | String of string
    //| Number of decimal
    | Number of int
    | Null
    | Boolean of bool
    | Array of list<Value>
    | Object of Map<string, Value>

  let example = Array [
    Null
    String "abc"
    Object (Map.ofList [
        "foo", Boolean false
        //"bar", Number 1.24m
        "bar", Number 3
    ])
  ]
