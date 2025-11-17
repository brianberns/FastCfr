module Tests

open Xunit

[<Fact>]
let ``Kuhn Poker`` () =
    let numGames = 500_000
    let chunkSize = 100
    let _, util = KuhnPoker.train numGames chunkSize
    Assert.Equal(-1f/18f, util, 0.002f)

[<Fact>]
let ``Kuhn Poker (three players)`` () =
    let numGames = 1_000_000
    let chunkSize = 100
    let _, utils = KuhnPoker3.train numGames chunkSize
    let expected = 1f/48f
    Assert.Equal(-expected, utils[1], 0.002f)
    Assert.Equal(expected, utils[0] + utils[2], 0.002f)
    Assert.InRange(utils[0], -2f * expected, expected)       // -1/24 to -1/48
    Assert.InRange(utils[2], 2f * expected, 3f * expected)   //  1/24 to  1/16

// https://cs.stackexchange.com/questions/169593/nash-equilibrium-details-for-leduc-holdem
[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 500_000
    let chunkSize = 100
    let _, util = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.085f, util, 0.01f)
