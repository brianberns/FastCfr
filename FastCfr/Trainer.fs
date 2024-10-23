namespace FastCfr

open MathNet.Numerics.LinearAlgebra

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

    /// Assume two-player, zero-sum game.
    let private numPlayers = 2

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let private updateReachProbabilities reachProbs activePlayer actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = activePlayer then
                    x * actionProb
                else x)

    /// Negates opponent's payoff (assuming a zero-zum game).
    let private getActiveUtility activePlayer (terminal, keyedInfoSets) =
        let utility =
            if terminal.PayoffPlayerIdx = activePlayer then
                terminal.Payoff
            else -terminal.Payoff
        utility, keyedInfoSets

    /// Evaluates the utility of the given game state via counter-
    /// factual regret minimization.
    let private cfr infoSetMap game =

        /// Top-level CFR loop.
        let rec loop reachProbs game =
            match game with
                | NonTerminal state -> loopNonTerminal state reachProbs
                | Terminal state -> state, Array.empty

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
                                |> loop reachProbs
                                |> getActiveUtility activePlayer)
                        |> Array.unzip
                DenseVector.ofSeq utilities,
                Array.concat keyedInfoSetArrays

                // utility of this info set is action utilities weighted by action probabilities
            let utility = actionUtilities * strategy

                // accumulate updated regrets and strategy
            let keyedInfoSets =
                let infoSet =
                    let regrets =
                        let opponent = (activePlayer + 1) % numPlayers
                        reachProbs[opponent] * (actionUtilities - utility)
                    let strategy =
                        reachProbs[activePlayer] * strategy
                    InformationSet.create regrets strategy
                [|
                    yield! keyedInfoSets
                    yield state.InfoSetKey, infoSet
                |]

            TerminalGameState.create activePlayer utility,
            keyedInfoSets

        let reachProbs = DenseVector.create numPlayers 1.0
        loop reachProbs game

    /// Trains using the given games.
    let train gameChunks =

        let infoSetMap, nGames, utilities =

                // start with no known info sets
            ((Map.empty, 0, 0.0), gameChunks)
                ||> Seq.fold (fun (infoSetMap, utilityCount, utilitySum) games ->

                        // evaluate each game in the given chunk
                    let utilities, updateChunks =
                        games
                            |> Array.Parallel.map (fun game ->
                                let state, keyedInfoSets = cfr infoSetMap game
                                assert(state.PayoffPlayerIdx = 0)
                                state.Payoff, keyedInfoSets)
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

                    infoSetMap,
                    utilityCount + utilities.Length,
                    utilitySum + Seq.sum utilities)

            // compute average utility per game
        let utility = utilities / float nGames
        utility, infoSetMap
