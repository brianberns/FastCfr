module Tests

open Xunit

[<Fact>]
let ``Kuhn Poiker`` () =
    let numGames = 50000
    let chunkSize = 250
    let util, _ = KuhnPoker.train numGames chunkSize
    Assert.Equal(-1./18., util, 0.005)

[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 50000
    let chunkSize = 250
    let util, _ = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.085, util, 0.005)
