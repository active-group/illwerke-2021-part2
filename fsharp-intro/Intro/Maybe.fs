namespace Intro

open System

module Maybe =
  // Beispiel: 3 Zahlen summieren, aber Zahlen sind Nutzereingabe
  let addUserInput0 () =
    printf "Bitte geben Sie eine Zahl ein: "
    match Int32.TryParse (Console.ReadLine ()) with
    | true, n1 ->
      printf "Bitte geben Sie eine Zahl ein: "
      match Int32.TryParse (Console.ReadLine ()) with
      | true, n2 ->
        printf "Bitte geben Sie eine Zahl ein: "
        match Int32.TryParse (Console.ReadLine ()) with
        | true, n3 ->
          printfn "Das Ergebnis der Berechnung ist %i" (n1 + n2 + n3)
        | _ -> ()
      | _ -> ()
    | _ -> ()

  // Übungsaufgabe:
  // REFACTORING!
  // - Mehrere Muster erkennbar, Seiteneffekte, kein Rueckgabewert, so nicht wirklich testbar durch Unit-Test

  type Maybe<'a> = Just of 'a | Nothing

  // Integer von der Kommandozeile einlesen
  let readInt (): Maybe<int> =
    printf "Bitte geben Sie eine Zahl ein: "
    let s = Console.ReadLine ()
    // Interop! F# trickst, damit man keinen Wert per Referenz uebergeben muss
    match Int32.TryParse s with
    | true, i -> Just i
    | _ -> Nothing
    // ^ syntaktischer Zucker fuer:
    //let mutable x = 0
    //if Int32.TryParse (s, &x) then Just x else Nothing
      
  // Noch Idee: drei Maybe-Inputs, um pure function zu haben?
  let addUserInput1 () =
    match readInt () with
    | Just n1 ->
      match readInt () with
      | Just n2 ->
        match readInt () with
        | Just n3 ->
          Just (n1 + n2 + n3)
        | Nothing -> Nothing
      | Nothing -> Nothing
    | Nothing -> Nothing

  // Umstaendlich zu schreiben; Einrueckung stoert Lesbarkeit
  // -> Computation Expressions

  // Wollen schreiben:
  //let addUserInput2 () =
  //    maybe {
  //        let! i1 = readInt ()
  //        let! i2 = readInt ()
  //        let! i3 = readInt ()
  //        return i1 + i2 + i3
  //    }

  let bind (mi : Maybe<int>) (f : int -> Maybe<int>): Maybe<int> =
    match mi with
    | Nothing -> Nothing
    | Just i -> f i

  let addUserInput2 () =
    bind (readInt ()) (fun n1 ->
      bind (readInt ()) (fun n2 ->
        bind (readInt ()) (fun n3 ->
          Just (n1 + n2 + n3))))

  // ^ Nur noch Einrueckung und Klammern stoeren etwas
  type MaybeBuilder () =
    member _.Bind (maybeVal, binder) = bind maybeVal binder
    member _.Return a = Just a

  let maybe = MaybeBuilder () // Konstruktor

  // endlich!
  let addUserInput3 () =
    maybe {
      let! n1 = readInt ()
      let! n2 = readInt ()
      let! n3 = readInt ()
      return n1 + n2 + n3
    }

  let runExample () =
    let result = addUserInput3 ()
    //let result = addUserInput0 (readInt ()) (readInt ()) (readInt ())
    printfn "Das Ergebnis der Berechnung ist %A" result

  // Kommentare:
  // - Bind muss nicht diesen Typ haben, kann auch wechseln (zeigen)
  // - Monaden muessen Gesetze erfuellen
  // - CEs koennen noch mehr (Applicative)
