namespace FastCfr

open System

open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra

#nowarn "57"

module Trainer =

    /// Obtains an info set corresponding to the given key.
    let inline private getInfoSet
        infoSetKey infoSetMap numActions =
        match Map.tryFind infoSetKey infoSetMap with
            | Some infoSet ->
                assert(infoSet.RegretSum.Count = numActions)
                assert(infoSet.StrategySum.Count = numActions)
                infoSet
            | None ->
                InformationSet.zero numActions   // first visit

    /// Assume two-player, zero-sum game.
    let numPlayers = 2

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let inline private updateReachProbabilities
        reachProbs activePlayer actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = activePlayer then
                    x * actionProb
                else x)

    /// Negates opponent's payoff (assuming a zero-zum game).
    let inline private getActiveUtility
        activePlayer (terminal, keyedInfoSets) =
        let utility =
            if terminal.PayoffPlayerIdx = activePlayer then
                terminal.Payoff
            else -terminal.Payoff
        utility, keyedInfoSets

    /// Evaluates the utility of the given game state via counter-
    /// factual regret minimization.
    let inline private cfr<'key, 'action, 'payoff
        when 'key : comparison
        and PayoffType<'payoff>>
        rng infoSetMap updatingPlayer (game : GameState<'key, 'action, 'payoff>) =

        /// Top-level CFR loop.
        let rec loop reachProbs game =
            match game with
                | NonTerminal state ->
                    if reachProbs |> Vector.forall ((=) 'payoff.Zero) then   // prune?
                        TerminalGameState.create state.ActivePlayerIdx 'payoff.Zero,
                        Array.empty
                    else
                        match state.LegalActions.Length with
                            | 0 -> failwith "No legal actions"
                            | 1 ->
                                state.AddAction(state.LegalActions[0])
                                    |> loop reachProbs
                            | _ -> loopNonTerminal state reachProbs
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

                // get utility of this info set
            let activePlayer = state.ActivePlayerIdx
            assert(activePlayer >= 0 && activePlayer < numPlayers)
            if activePlayer = updatingPlayer then

                    // get utility of each action
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
                let utility = Vector.(*)(actionUtilities, strategy)

                    // accumulate updated regrets and strategy
                let keyedInfoSets =
                    let infoSet =
                        let regrets =
                            let opponent = (activePlayer + 1) % numPlayers
                            let diff = Vector.(-)(actionUtilities, utility)
                            Vector.(*)(reachProbs[opponent], diff)
                        let strategy =
                            Vector.(*)(reachProbs[activePlayer], strategy)
                        InformationSet.create regrets strategy
                    [|
                        yield! keyedInfoSets
                        yield state.InfoSetKey, infoSet
                    |]

                TerminalGameState.create activePlayer utility,
                keyedInfoSets

            else
                    // sample a single action according to the strategy
                let utility, keyedInfoSets =
                    let probMass =
                        strategy
                            |> Seq.cast<float>   // ugh
                            |> Seq.toArray
                    Categorical.Sample(rng, probMass)
                        |> Array.get state.LegalActions
                        |> state.AddAction
                        |> loop reachProbs
                        |> getActiveUtility activePlayer
                TerminalGameState.create activePlayer utility,
                keyedInfoSets

        let reachProbs = DenseVector.create numPlayers LanguagePrimitives.GenericOne
        let state, keyedInfoSets = loop reachProbs game
        state.Payoff, keyedInfoSets

    /// Updates information sets.
    let inline private update infoSetMap updateChunks =
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

    /// Trains using the given games.
    let inline train<'key, 'action, 'payoff
        when 'key : comparison
        and PayoffType<'payoff>>
        gameChunks =

        let infoSetMap, nGames, utilities =

                // start with no known info sets
            ((Map.empty, 0, 'payoff.Zero), gameChunks)
                ||> Seq.fold (
                    fun (infoSetMap, utilityCount, utilitySum) games ->

                        // evaluate each game in the given chunk
                    let utilities, updateChunks =
                        if Array.length games % 2 <> 0 then
                            failwith "Chunk contains an odd number of games"
                        games
                            |> Array.Parallel.mapi (
                                fun iGame (game : GameState<'key, 'action, 'payoff>) ->
                                    let rng = Random()
                                    let updatingPlayer = iGame % numPlayers
                                    cfr rng infoSetMap updatingPlayer game)
                            |> Array.unzip

                        // update info sets
                    update infoSetMap updateChunks,
                    utilityCount + utilities.Length,
                    utilitySum + Seq.sum utilities)

            // compute average utility per game
        let utility = 'payoff.DivideByInt(utilities, nGames)
        utility, infoSetMap
