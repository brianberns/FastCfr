module Tests

open Xunit

[<Fact>]
let ``Kuhn Poker`` () =
    let numGames = 500000
    let chunkSize = 1000
    let util, _ = KuhnPoker.train numGames chunkSize
    Assert.Equal(-1.f/18.f, util, 0.02f)

[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 500000
    let chunkSize = 1000
    let util, _ = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.085, util, 0.02)
