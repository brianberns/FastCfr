module Tests

open Xunit

[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 50000
    let chunkSize = 250
    let util, _ = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.08018, util, 0.000005)
