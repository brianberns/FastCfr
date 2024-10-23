namespace FastCfr.Test

open System
open FastCfr

module Program =

    let run () =

            // train
        let numGames = 50000
        let chunkSize = 250
        printfn $"Running Leduc Hold'em parallel CFR for {numGames} games"
        printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}\n"
        let util, infoSetMap = LeducHoldem.train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"

            // strategy
        printfn "Strategy:"
        for (KeyValue(key, infoSet)) in infoSetMap do
            let actions =
                key
                    |> Seq.where Char.IsLower
                    |> Seq.toArray
                    |> String
                    |> LeducHoldem.getLegalActions
            let str =
                let strategy =
                    InformationSet.getAverageStrategy infoSet
                (strategy.ToArray(), actions)
                    ||> Array.map2 (fun prob action ->
                        sprintf "%s: %0.5f" action prob)
                    |> String.concat ", "
            printfn $"%-11s{key}:    {str}"

    let timer = Diagnostics.Stopwatch.StartNew()
    run ()
    printfn ""
    printfn $"Elapsed time: {timer}"
