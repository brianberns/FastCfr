namespace FastCfr.Test

open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open FastCfr

(*
BenchmarkDotNet v0.15.7, Windows 11 (10.0.26200.7171/25H2/2025Update/HudsonValley2)
12th Gen Intel Core i9-12900 2.40GHz, 1 CPU, 24 logical and 16 physical cores
.NET SDK 10.0.100
  [Host]     : .NET 8.0.22 (8.0.22, 8.0.2225.52707), X64 RyuJIT x86-64-v3 DEBUG
  DefaultJob : .NET 8.0.22 (8.0.22, 8.0.2225.52707), X64 RyuJIT x86-64-v3


| Method      | NumGames | ChunkSize | Mean     | Error   | StdDev  |
|------------ |--------- |---------- |---------:|--------:|--------:|
| LeducHoldem | 100000   | 100       | 727.8 ms | 5.78 ms | 5.13 ms |
*)
type Benchmark() =

    [<Params(100_000)>]
    member val NumGames = 0 with get, set

    [<Params(100)>]
    member val ChunkSize = 0 with get, set

    [<Benchmark>]
    member this.LeducHoldem() =
        LeducHoldem.train this.NumGames this.ChunkSize

module Program =

    let runLeduc () =

            // train
        let numGames = 100_000
        let chunkSize = 100
        printfn $"Running Leduc Hold'em parallel Monte Carlo CFR for {numGames} games"
        printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}\n"
        let timer = Diagnostics.Stopwatch.StartNew()
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

        printfn ""
        printfn $"Elapsed time: {timer}"

    let runKuhn () =

            // train
        let numGames = 100_000
        let chunkSize = 100
        printfn $"Running Kuhn Poker parallel CFR for {numGames} games"
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

    BenchmarkRunner.Run(typeof<Benchmark>) |> ignore
    // runLeduc ()
