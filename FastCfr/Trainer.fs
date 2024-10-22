namespace FastCfr

open MathNet.Numerics.LinearAlgebra

type GameState =
    {
        IsTerminal : bool
        TerminalValueOpt : Option<float>
        LegalActions : int[]
        InfoSetKey : string
        ActivePlayerIdx : int
        AddAction : int -> GameState
    }

module Trainer =

    /// Obtains an info set corresponding to the given key.
    let private getInfoSet infoSetKey infoSetMap numActions =
        match Map.tryFind infoSetKey infoSetMap with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet
            | None ->
                InformationSet.zero numActions   // first visit

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let private updateReachProbabilities reachProbs activePlayer actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = activePlayer then
                    x * actionProb
                else x)

    /// Negates opponent's utilties (assuming a zero-zum game).
    let private getActiveUtilities utilities =
        utilities
            |> Seq.map (~-)
            |> DenseVector.ofSeq

    let private numPlayers = 2

    /// Evaluates the utility of the given game state via counter-
    /// factual regret minimization.
    let private cfr infoSetMap (state : GameState) =

        /// Top-level CFR loop.
        let rec loop state reachProbs =

                // game is over?
            match state.TerminalValueOpt with
                | None -> loopNonTerminal state reachProbs
                | Some payoff -> payoff, Array.empty

        /// Recurses for non-terminal game state.
        and loopNonTerminal (state : GameState) reachProbs =

                // get player's current strategy for this info set
            let strategy =
                let infoSet =
                    getInfoSet
                        state.InfoSetKey
                        infoSetMap
                        state.LegalActions.Length
                InformationSet.getStrategy infoSet

                // get utility of each action
            let actionUtilities, keyedInfoSets =
                let utilities, keyedInfoSetArrays =
                    (state.LegalActions, strategy.ToArray())
                        ||> Array.map2 (fun action actionProb ->
                            let reachProbs =
                                updateReachProbabilities
                                    reachProbs
                                    state.ActivePlayerIdx
                                    actionProb
                            let state = state.AddAction(action)
                            loop state reachProbs)
                        |> Array.unzip
                getActiveUtilities utilities,
                Array.concat keyedInfoSetArrays

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy

                // accumulate updated regrets and strategy
            let keyedInfoSets =
                let infoSet =
                    let regrets =
                        let opponent =
                            (state.ActivePlayerIdx + 1) % numPlayers
                        reachProbs[opponent] * (actionUtilities - utility)
                    let strategy =
                        reachProbs[state.ActivePlayerIdx] * strategy
                    InformationSet.create regrets strategy
                [|
                    yield! keyedInfoSets
                    yield state.InfoSetKey, infoSet
                |]

            utility, keyedInfoSets

        [| 1.0; 1.0 |]
            |> DenseVector.ofArray
            |> loop state

    /// Trains for the given number of iterations.
    let train numIterations =

        let utilities, infoSetMap =

                // each iteration evaluates a chunk of deals
            let dealChunks =
                let permutations =
                    LeducHoldem.deck
                        |> List.permutations
                        |> Seq.map (fun deck ->
                            Seq.toArray deck[0..1], deck[2])
                        |> Seq.toArray
                let chunkSize = 250
                seq {
                    for i = 0 to numIterations - 1 do
                        yield permutations[i % permutations.Length]
                } |> Seq.chunkBySize chunkSize

                // start with no known info sets
            (Map.empty, dealChunks)
                ||> Seq.mapFold (fun infoSetMap deals ->

                        // evaluate each deal in the given chunk
                    let utilities, updateChunks =
                        deals
                            |> Array.Parallel.map
                                (fun (playerCards, communityCard) ->
                                    cfr infoSetMap playerCards communityCard)
                            |> Array.unzip

                        // update info sets
                    let infoSetMap =
                        seq {
                            yield! Map.toSeq infoSetMap
                            for updates in updateChunks do
                                yield! updates
                        }
                            |> Seq.groupBy fst
                            |> Seq.map (fun (key, group) ->
                                let sum =
                                    group
                                        |> Seq.map snd
                                        |> Seq.reduce (+)   // accumulate regrets and strategies
                                key, sum)
                            |> Map

                    Seq.sum utilities, infoSetMap)

            // compute average utility per deal
        let utility =
            Seq.sum utilities / float numIterations
        utility, infoSetMap
