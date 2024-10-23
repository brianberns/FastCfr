module Tests

open Xunit

[<Fact>]
let ``Leduc Hold'em`` () =
    let numGames = 500000
    let chunkSize = 250
    let util, _ = LeducHoldem.train numGames chunkSize
    Assert.Equal(-0.08, util, 0.01)
