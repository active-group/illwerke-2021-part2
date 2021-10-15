namespace Intro

module Tests =
  open Xunit

  [<Fact>]
  let ``My test`` () =
    Assert.Equal (Foo.f 394, -1)

  open FsUnit.Xunit

  [<Fact>]
  let ``bla with FsUnit for a nicer API`` () =
    Foo.f 5 |> should equal -1

  [<Theory>]
  [<InlineData 2>]
  [<InlineData 5>]
  [<InlineData 7>]
  [<InlineData -5>]
  let ``with theory`` n =
    Foo.f n |> should equal -1