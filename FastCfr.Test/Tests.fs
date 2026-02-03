module Tests

open FastCfr
open Xunit

[<Fact>]
let ``Kuhn Poker`` () =
    let numGames = 500_000
    let chunkSize = 100
    let _, util = KuhnPoker.train numGames chunkSize
    Assert.Equal(-1.0/18.0, util, 0.002)

[<Fact>]
let ``Kuhn Poker (three players)`` () =
    let numGames = 1_500_000
    let chunkSize = 100
    let infoSetMap, utils = KuhnPoker3.train numGames chunkSize
    let expected = 1.0/48.0
    Assert.Equal(-expected, utils[1], 0.005)
    Assert.Equal(expected, utils[0] + utils[2], 0.005)
    Assert.InRange(utils[0], -2.0 * expected, expected)       // -1/24 to -1/48
    Assert.InRange(utils[2], 2.0 * expected, 3.0 * expected)   //  1/24 to  1/16
    for card in KuhnPoker3.deck do
        let strategy =
            InformationSet.getAverageStrategy infoSetMap[card]
        Assert.Equal(1.0, strategy[1], 0.03)   // first player always checks on the first action

// https://cs.stackexchange.com/questions/169593/nash-equilibrium-details-for-leduc-holdem
[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 500_000
    let chunkSize = 100
    let _, util = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.085, util, 0.01)
