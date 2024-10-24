namespace FastCfr.Test

open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open FastCfr

(*
BenchmarkDotNet v0.14.0, Windows 11 (10.0.22631.4391/23H2/2023Update/SunValley3)
12th Gen Intel Core i9-12900, 1 CPU, 24 logical and 16 physical cores
.NET SDK 8.0.403
  [Host]     : .NET 8.0.10 (8.0.1024.46610), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 8.0.10 (8.0.1024.46610), X64 RyuJIT AVX2


| Method      | NumGames | ChunkSize | Mean     | Error    | StdDev   |
|------------ |--------- |---------- |---------:|---------:|---------:|
| LeducHoldem | 500000   | 1000      | 858.2 ms | 16.93 ms | 16.62 ms |
*)
type Benchmark() =

    [<Params(500_000)>]
    member val NumGames = 0 with get, set

    [<Params(1000)>]
    member val ChunkSize = 0 with get, set

    [<Benchmark>]
    member this.LeducHoldem() =
        LeducHoldem.train this.NumGames this.ChunkSize

module Program =

    let runLeduc () =

            // train
        let numGames = 5_000_000
        let chunkSize = 1000
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
        let numGames = 50_000
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
    runKuhn ()
