namespace FastCfr

open MathNet.Numerics.LinearAlgebra

type GameState<'action> =
    {
        /// If the game is over, payoff for the first player.
        TerminalValueOpt : Option<float>
        LegalActions : 'action[]
        InfoSetKey : string
        ActivePlayerIdx : int
        AddAction : 'action -> GameState<'action>
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

    let private numPlayers = 2

    /// Evaluates the utility of the given game state via counter-
    /// factual regret minimization.
    let private cfr infoSetMap state =

        /// Top-level CFR loop.
        let rec loop state reachProbs =

                // game is over?
            match state.TerminalValueOpt with
                | None -> loopNonTerminal state reachProbs
                | Some payoff -> payoff, Array.empty

        /// Recurses for non-terminal game state.
        and loopNonTerminal state reachProbs =

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
                DenseVector.ofSeq utilities,
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
    let train numIterations chunkSize states =

        let utilities, infoSetMap =

            let chunks = Seq.chunkBySize chunkSize states

                // start with no known info sets
            (Map.empty, chunks)
                ||> Seq.mapFold (fun infoSetMap chunk ->

                        // evaluate each deal in the given chunk
                    let utilities, updateChunks =
                        chunk
                            |> Array.Parallel.map (cfr infoSetMap)
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
