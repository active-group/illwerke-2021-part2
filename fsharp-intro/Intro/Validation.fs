namespace Intro

module Validation =
  // Inspiriert vom Microsoft-Artikel zu Result und Railway-oriented programming:
  // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/results
  // und Blogpost von M. Sperber auf funktionale-programmierung.de

  // Klassisches Beispiel: Formulareingabe: es können mehrere Werte
  // gleichzeitig "falsch" sein.
  type Validation<'a, 'error> = Fine of 'a | Errors of list<'error>

  // Bemerkung: Es gibt noch den Typ Choice in versch. Ausprägungen

  type Request =
    { Name: string
      Email: string
    }

  // Anfangs ähnlich zu Result oder Option, aber wir wollen mehrere
  // Fehler auf einmal "tracken".  Applikativ wie bei den Dekodierern!

  // Hilfsfunktion
  let error err = Errors [err]

  // Nur bestimmte Namen sind erlaubt
  let validateName request =
    match request.Name with
    | null -> error "No name given"
    | "" -> error "Empty name"
    | "bananas" -> error "Bananas is not a name"
    | name -> Fine name

  let validateEmail request =
    match request.Email with
    | null -> error "No email given"
    | "" -> error "Email is empty"
    | email when email.EndsWith "bananas.com" ->
      error "No email from bananas.com is allowed"
    | email -> Fine email

  // "Gleichzeitiges" Validieren, analog zum Dekodieren: Wollen alle Fehler als Rückgabe

  let map (f: 'a -> 'b) (x: Validation<'a, 'error>) =
    match x with
    | Fine a -> Fine (f a)
    | Errors errs -> Errors errs

  type ValidateBuilder () =
    member _.BindReturn (x: Validation<'a, 'error>, f: 'a -> 'b): Validation<'b, 'error> =
      map f x
    member _.MergeSources (x, y) =
      match x, y with
      | Fine a, Fine b -> Fine (a, b)
      | Errors errs, Fine _ -> Errors errs
      | Fine _, Errors errs -> Errors errs
      | Errors errs1, Errors errs2 -> errs1 @ errs2 |> Errors
    member _.ReturnFrom x = x
    member _.Return a = Fine a
    //member _.Zero = Fine ()
    member _.Bind (valA, binder) =
      match valA with
      | Fine a -> binder a
      | Errors errs -> Errors errs

  let validate = ValidateBuilder ()

  // Noch besser: neuen, anderen Typ zurückgeben, der den Request wrappt
  //type ValidatedRequest = ValidatedRequest of Request

  // Validiere Name und Email eines Requests
  let validateRequest request =
    validate {
      let! name = validateName request
      and! email = validateEmail request
      //return ValidatedRequest request
      return request
    }

  let validateRequestM request =
    validate {
      let! name = validateName request
      // Achtung: hier kein and!
      let! email = validateEmail request
      return request
    }

  //let validatePositiveInt (i: int) =
  //  validate {
  //    if i > 0
  //      then return ()
  //      else return! error "This is bad"
  //  }

  // FIXME
  //let validateRequestAndPositiveInt request i =
  //  validate {
  //    // Erfordert Combine und Zero
  //    return! validateRequest request
  //    do! validatePositiveInt i
  //  }

  let runExample () =
    let req = { Name = ""; Email = null }
    // Was nicht funktioniert: monadische Variante: Ausstieg beim ersten Fehler ->
    // syntaktisch ähnliche CEs führen zu sehr verschiedenem Code ->
    // wir empfehlen, Validierung selbst zu bauen und nur das Nötigste zu implementieren.
    //match validateRequestM req with
    match validateRequest req with
    | Fine _ -> printfn "all good"
    | Errors errs -> printfn "got some errors: %A" errs

  // Anmerkungen:
  //
  // - "Parse, don't validate" erwähnen
  // - FSToolkit (https://github.com/demystifyfp/FsToolkit.ErrorHandling/) hat eingebaute CE,
  //   aber auch leider monadisches Bind. Das will man nicht, da ein let!-and!-Konstrukt nun applikativ
  //   abläuft, ein (versehentliches) let!-let! aber auf Bind dispatcht und damit Fehler verwirft.
  //
  // Fazit: Lohnt sich aus unserer Sicht nicht, hier etwas Externes heranzuziehen, da leicht
  // selbstgeschrieben (u.U. ist Implementierung als simpler Alias "type Validation<'a, 'err> = Result<'a, list<'err>>"
  // noch etwas angenehmer).
