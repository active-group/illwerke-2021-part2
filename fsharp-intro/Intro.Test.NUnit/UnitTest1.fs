namespace Intro

module Tests =

  open NUnit.Framework

  [<SetUp>]
  let Setup () =
    // Called before every test in this module
    printfn "in setup"
    ()

  open FsUnit

  [<Test>]
  let ``my longer-named test`` () =
    Foo.f 17 |> should equal -1

  // BUT!

  // [<Test>]
  // let ``bad test, as it doesn't type-check`` () =
  //   Foo.f 3 |> should equal "a"

  // better than FsUnit
  open FsUnitTyped

  [<Test>]
  let ``my longer-named test, with typed API`` () =
    Foo.f 17 |> shouldEqual -1

  // with classes/fixtures

  // -> set this if you want to have the class instantiated for every
  // test case:
  [<FixtureLifeCycle(LifeCycle.InstancePerTestCase)>]
  [<TestFixture>]
  type SomeTestClass () =
    let classState = "abc"
    let mutable mutClassState = "blubber"

    [<SetUp>]
    member _.SetUp () =
      // Called before every test in this class
      printfn "in class setup\nthe value of mutClassState is: %s" mutClassState

    [<Test>]
    member _.``foo test bar`` () =
      classState |> shouldEqual "abc"

    [<Test>]
    member _.``some other member test that mutates state`` () =
      mutClassState <- "hi there"
      mutClassState |> shouldEqual "hi there"

    [<Test>]
    member _.``some other member test that uses the same state`` () =
      mutClassState |> shouldEqual "blubber"

    [<TearDown>]
    member _.TearDown () =
      // Called after every test in this class
      printfn "in class teardown"

    [<OneTimeSetUp>]
    static member OneTimeSetUp () =
      printfn "in one-time setup"

    [<OneTimeTearDown>]
    static member OneTimeTearDown () =
      printfn "in one-time teardown"

  // Property tests

  open FsCheck.NUnit

  // ACHTUNG: findet den Test nicht!
  //let bla (xs: list<bool>) = List.rev xs = xs

  //[<Property>]
  //let ``double negation is identity`` = bla

  [<Property>]
  let ``double negation is identity`` (b: bool) =
    not (not b) = b
