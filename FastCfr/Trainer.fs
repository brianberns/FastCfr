namespace FastCfr

open MathNet.Numerics.LinearAlgebra

module Array =

    module Parallel =

        /// Maps the given arrays using the given function .
        let map2 mapping array1 array2 =
            Array.zip array1 array2
                |> Array.Parallel.map (fun (val1, val2) ->
                    mapping val1 val2)

/// State maintained during training.
type TrainerState<'key when 'key : comparison> =
    {
        /// Maps each information set seen so far by its key.
        InfoSetMap : Map<'key, InformationSet>

        /// Sum of per-player utilties so far.
        UtilitySum : Vector<float32>

        /// Number of games played so far.
        NumGames : int
    }

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
                        ||> Array.Parallel.map2 (fun action actionProb ->
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
            let utility = strategy * actionUtilities
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

            // match and reduce updates
        let updates =
            updateChunks
                |> Seq.concat
                |> Seq.groupBy fst
                |> Seq.map (fun (key, keyedInfoSets) ->
                    let sum =
                        keyedInfoSets
                            |> Seq.map snd
                            |> Seq.reduce (+)   // avoid parallelism for determistic floating point math
                    key, sum)

            // merge updates into existing info sets
        (infoSetMap, updates)
            ||> Seq.fold (fun infoSetMap (key, infoSet) ->
                let infoSet =
                    infoSetMap
                        |> Map.tryFind key
                        |> Option.map ((+) infoSet)
                        |> Option.defaultValue infoSet
                Map.add key infoSet infoSetMap) 

    /// Trains using the given games, yielding the state
    /// after each chunk of games.
    let trainScan numPlayers gameChunks =

            // start with no known info sets
        let state =
            {
                InfoSetMap = Map.empty
                UtilitySum = DenseVector.zero numPlayers
                NumGames = 0
            }
        (state, gameChunks)
            ||> Seq.scan (fun state games ->

                    // evaluate each game in the given chunk
                let utilities, updateChunks =
                    games
                        |> Array.Parallel.map (
                            cfr numPlayers state.InfoSetMap)
                        |> Array.unzip

                    // update info sets
                {
                    InfoSetMap =
                        update state.InfoSetMap updateChunks
                    UtilitySum =
                        Array.fold (+) state.UtilitySum utilities
                    NumGames =
                        state.NumGames + games.Length
                })
            |> Seq.skip 1   // skip initial state

    /// Trains using the given games.
    let train numPlayers gameChunks =

            // train to final result
        let state =
            trainScan numPlayers gameChunks
                |> Seq.last

            // compute average utility per game
        let utilities =
            state.UtilitySum
                |> Vector.map (fun utility ->
                    utility / float32 state.NumGames)
        state.InfoSetMap, utilities

    /// Trains a two-player game using the given games.
    let trainTwoPlayer gameChunks =
        let infoSetMap, utilities = train 2 gameChunks
        infoSetMap, utilities[0]
