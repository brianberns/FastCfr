namespace FastCfr

open MathNet.Numerics.LinearAlgebra

/// Maps an info set key to its info set.
type KeyedInformationSet<'key, 'payoff
    when 'key : comparison
    and PayoffType<'payoff>> = 'key * InformationSet<'payoff>

/// Maps each info set key to its info set.
type InformationSetMap<'key, 'payoff
    when 'key : comparison
    and PayoffType<'payoff>> = Map<'key, InformationSet<'payoff>>

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

    /// Updates the active player's reach probability to reflect
    /// the probability of an action.
    let inline private updateReachProbabilities
        reachProbs activePlayer actionProb =
        reachProbs
            |> Vector.mapi (fun i x ->
                if i = activePlayer then
                    x * actionProb
                else x)

    /// Gets opponents' reach probability.
    let inline getOpponentsReachProbability<'payoff
        when PayoffType<'payoff>>
        (reachProbs : Vector<'payoff>) activePlayer =
        reachProbs
            |> Seq.mapi (fun i x ->
                if i = activePlayer then 'payoff.One
                else x)
            |> Seq.reduce (*)

    /// Evaluates the utility of the given game state via counter-
    /// factual regret minimization.
    let inline private cfr<'key, 'action, 'payoff
        when 'key : comparison
        and PayoffType<'payoff>>
        numPlayers
        (infoSetMap : InformationSetMap<'key, 'payoff>)
        (game : GameState<'key, 'action, 'payoff>) :
            Vector<'payoff> * KeyedInformationSet<'key, 'payoff>[] =

        /// Top-level CFR loop.
        let rec loop reachProbs game :
            Vector<'payoff> * KeyedInformationSet<'key, 'payoff>[] =
            match game with
                | NonTerminal state ->
                    if Vector.forall ((=) 'payoff.Zero) reachProbs then   // prune?
                        DenseVector.zero<'payoff> numPlayers,
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
        and loopNonTerminal state reachProbs :
            Vector<'payoff> * KeyedInformationSet<'key, 'payoff>[]  =

                // per-player probabilities of reaching this state
            assert(reachProbs.Count = numPlayers)
            assert(
                reachProbs
                    |> Vector.forall (fun prob ->
                        prob >= 'payoff.Zero && prob <= 'payoff.One))

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
            let utility = Matrix.(*)(actionUtilities, strategy)
            assert(utility.Count = numPlayers)

                // accumulate updated regrets and strategy
            let keyedInfoSets =
                let infoSet =
                    let regrets =
                        let reachProb =
                            getOpponentsReachProbability
                                reachProbs activePlayer
                        let diff =
                            Vector.(-)(
                                actionUtilities[0.., activePlayer],
                                utility[activePlayer])
                        Vector.(*)(reachProb, diff)
                    let strategy =
                        Vector.(*)(reachProbs[activePlayer], strategy)
                    InformationSet.create regrets strategy
                [|
                    yield! keyedInfoSets
                    yield state.InfoSetKey, infoSet
                |]

            utility, keyedInfoSets

        let reachProbs =
            DenseVector.create numPlayers 'payoff.One
        loop reachProbs game

    /// Updates information sets.
    let inline private update
        (infoSetMap : InformationSetMap<_, _>)
        updateChunks :
            InformationSetMap<_, _> =
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
    let inline trainScan<'key, 'action, 'payoff
        when 'key : comparison
        and PayoffType<'payoff>>
        numPlayers
        gameChunks :
            seq<InformationSetMap<'key, 'payoff> * int (*nGames*) * Vector<'payoff>> =

            // start with no known info sets
        let infoSetMap : InformationSetMap<'key, 'payoff> = Map.empty
        let utilitySum = DenseVector.zero<'payoff> numPlayers
        ((infoSetMap, 0, utilitySum), gameChunks)
            ||> Seq.scan (fun (infoSetMap, utilityCount, utilitySum) games ->

                    // evaluate each game in the given chunk
                let utilities, updateChunks =
                    games
                        |> Array.Parallel.mapi (
                            fun
                                iGame
                                (game : GameState<'key, 'action, 'payoff>) ->
                                cfr numPlayers infoSetMap game)
                        |> Array.unzip

                    // update info sets
                let infoSetMap = update infoSetMap updateChunks
                let nGames = utilityCount + utilities.Length
                let utilities = utilitySum + Array.reduce (+) utilities
                infoSetMap, nGames, utilities)
            |> Seq.skip 1   // skip initial state

    /// Trains using the given games.
    let inline train<'key, 'action, 'payoff
        when 'key : comparison
        and PayoffType<'payoff>>
        numPlayers
        gameChunks :
            InformationSetMap<'key, 'payoff> * Vector<'payoff> =

            // train to final result
        let infoSetMap, nGames, utilities =
            trainScan<'key, 'action, 'payoff>
                numPlayers gameChunks
                |> Seq.last

            // compute average utility per game
        let utilities =
            utilities
                |> Vector.map (fun utility ->
                    'payoff.DivideByInt(utility, nGames))
        infoSetMap, utilities
