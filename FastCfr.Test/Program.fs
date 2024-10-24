namespace FastCfr.Test

open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open FastCfr

type Benchmark() =

    [<Params(500_000)>]
    member val NumGames = 0 with get, set

    [<Params(250)>]
    member val ChunkSize = 0 with get, set

    [<Benchmark>]
    member this.KuhnPoker() =
        KuhnPoker.train this.NumGames this.ChunkSize

    [<Benchmark>]
    member this.LeducHoldem() =
        LeducHoldem.train this.NumGames this.ChunkSize

module Program =

    let run () =

            // train
        let numGames = 500_000
        let chunkSize = 250
        printfn $"Running Kuhn Poker parallel Monte Carlo CFR for {numGames} games"
        printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}\n"
        let timer = Diagnostics.Stopwatch.StartNew()
        let util, infoSetMap = KuhnPoker.train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"

            // strategy
        printfn "Strategy:"
        for (KeyValue(key, infoSet)) in infoSetMap do
            let str =
                let strategy =
                    InformationSet.getAverageStrategy infoSet
                (strategy.ToArray(), KuhnPoker.actions)
                    ||> Array.map2 (fun prob action ->
                        sprintf "%s: %0.5f" action prob)
                    |> String.concat ", "
            printfn $"%-11s{key}:    {str}"

        printfn ""
        printfn $"Elapsed time: {timer}"

    // BenchmarkRunner.Run(typeof<Benchmark>) |> ignore
    run ()
