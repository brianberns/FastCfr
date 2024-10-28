module Tests

open Xunit

[<Fact>]
let ``Kuhn Poker`` () =
    let numGames = 500000
    let chunkSize = 1000
    let util, _ = KuhnPoker.train numGames chunkSize
    Assert.Equal(-1./18., util, 0.02)

[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 500000
    let chunkSize = 1000
    let util, _ = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.085, util, 0.02)
