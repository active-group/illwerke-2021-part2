// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

// Funktioniert auch:
//module F = illwerke.A.B.Foo.File2
//open F

// Ebenfalls (mit File2.hi unten):
//open illwerke.A.B.Foo

open illwerke.A.B.Foo.File2

[<EntryPoint>]
let main argv =
    let message = hi
    printfn "Hello world %s" message
    0 // return an integer exit code