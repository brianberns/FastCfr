namespace FastCfr

open MathNet.Numerics.LinearAlgebra

module Trainer =

    /// Obtains an info set corresponding to the given key.
    let private getInfoSet
        infoSetKey infoSetMap numActions =
        match Map.tryFind infoSetKey infoSetMap with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet
            | None ->
                InformationSet.zero numActions   // first visit

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let private updateReachProbabilities
        reachProbs activePlayer actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = activePlayer then
                    x * actionProb
                else x)

    /// Gets opponents' reach probability.
    let getOpponentsReachProbability
        (reachProbs : Vector<_>) activePlayer =
        reachProbs
            |> Seq.mapi (fun i x ->
                if i = activePlayer then 1f
                else x)
            |> Seq.reduce (*)

    /// Evaluates the utility of the given game state via counter-
    /// factual regret minimization.
    let private cfr numPlayers infoSetMap game =

        /// Top-level CFR loop.
        let rec loop reachProbs game =
            match game with
                | NonTerminal state ->
                    if Vector.forall ((=) 0f) reachProbs then   // prune?
                        DenseVector.zero numPlayers,
                        Array.empty
                    else
                        match state.LegalActions.Length with
                            | 0 -> failwith "No legal actions"
                            | 1 ->
                                state.AddAction(state.LegalActions[0])
                                    |> loop reachProbs
                            | _ -> loopNonTerminal state reachProbs
                | Terminal state ->
                    DenseVector.ofArray state.Payoffs,
                    Array.empty

        /// Recurses for non-terminal game state.
        and loopNonTerminal state reachProbs =

                // per-player probabilities of reaching this state
            assert(reachProbs.Count = numPlayers)
            assert(
                reachProbs
                    |> Vector.forall (fun prob ->
                        prob >= 0f && prob <= 1f))

                // get current strategy for this info set
            let strategy =
                let infoSet =
                    getInfoSet
                        state.InfoSetKey
                        infoSetMap
                        state.LegalActions.Length
                InformationSet.getStrategy infoSet

                // get utility of each action
            let activePlayer = state.ActivePlayerIdx
            assert(activePlayer >= 0 && activePlayer < numPlayers)
            let actionUtilities, keyedInfoSets =
                let utilities, keyedInfoSetArrays =
                    (state.LegalActions, strategy.AsArray())
                        ||> Array.map2 (fun action actionProb ->
                            let reachProbs =
                                updateReachProbabilities
                                    reachProbs
                                    activePlayer
                                    actionProb
                            state.AddAction(action)
                                |> loop reachProbs)
                        |> Array.unzip
                DenseMatrix.ofRowSeq utilities,
                Array.concat keyedInfoSetArrays

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy
            assert(utility.Count = numPlayers)

                // accumulate updated regrets and strategy
            let keyedInfoSets =
                let infoSet =
                    let regrets =
                        let reachProb =
                            getOpponentsReachProbability
                                reachProbs activePlayer
                        let diff =
                            actionUtilities[0.., activePlayer]
                                - utility[activePlayer]
                        reachProb * diff
                    let strategy =
                        reachProbs[activePlayer] * strategy
                    InformationSet.create regrets strategy
                [|
                    yield! keyedInfoSets
                    yield state.InfoSetKey, infoSet
                |]

            utility, keyedInfoSets

        let reachProbs =
            DenseVector.create numPlayers 1f
        loop reachProbs game

    /// Updates information sets.
    let private update infoSetMap updateChunks =
        Array.append
            (Map.toArray infoSetMap)
            (Array.concat updateChunks)
            |> Array.Parallel.groupBy fst
            |> Array.Parallel.map (fun (key, group) ->
                let sum =
                    group
                        |> Array.map snd
                        |> Array.reduce (+)   // accumulate regrets and strategies
                key, sum)
            |> Map

    /// Trains using the given games, yielding the state
    /// after each chunk of games.
    let trainScan numPlayers gameChunks =

            // start with no known info sets
        let infoSetMap = Map.empty
        let utilitySum = DenseVector.zero numPlayers
        ((infoSetMap, 0, utilitySum), gameChunks)
            ||> Seq.scan (fun (infoSetMap, utilityCount, utilitySum) games ->

                    // evaluate each game in the given chunk
                let utilities, updateChunks =
                    games
                        |> Array.Parallel.map (
                            cfr numPlayers infoSetMap)
                        |> Array.unzip

                    // update info sets
                let infoSetMap = update infoSetMap updateChunks
                let nGames = utilityCount + utilities.Length
                let utilities = utilitySum + Array.reduce (+) utilities
                infoSetMap, nGames, utilities)
            |> Seq.skip 1   // skip initial state

    /// Trains using the given games.
    let train numPlayers gameChunks =

            // train to final result
        let infoSetMap, nGames, utilities =
            trainScan numPlayers gameChunks
                |> Seq.last

            // compute average utility per game
        let utilities =
            utilities
                |> Vector.map (fun utility ->
                    utility / float32 nGames)
        infoSetMap, utilities

    /// Trains a two-player game using the given games.
    let trainTwoPlayer gameChunks =
        let infoSetMap, utilities = train 2 gameChunks
        infoSetMap, utilities[0]
