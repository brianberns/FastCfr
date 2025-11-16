module Tests

open Xunit

[<Fact>]
let ``Kuhn Poker`` () =
    let numGames = 500_000
    let chunkSize = 100
    let util, _ = KuhnPoker.train numGames chunkSize
    Assert.Equal(-1.f/18.f, util, 0.002f)

// https://cs.stackexchange.com/questions/169593/nash-equilibrium-details-for-leduc-holdem
[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 500_000
    let chunkSize = 100
    let util, _ = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.085, util, 0.01)
