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

    /// Evaluates the utility of the given deal via counterfactual
    /// regret minimization.
    let private cfr infoSetMap playerCards communityCard =

        /// Top-level CFR loop.
        let rec loop (history : string) reachProbs =
            let rounds = history.Split('d')

                // game is over?
            if LeducHoldem.isTerminal rounds then
                let payoff =
                    LeducHoldem.getPayoff
                        playerCards
                        communityCard
                        rounds
                float payoff, Array.empty

                // first round is over?
            elif LeducHoldem.isRoundEnd (Array.last rounds) then
                let sign =
                    match history with
                        | "xbc" | "brc" -> -1.0
                        | _ -> 1.0   // active player to play again
                let utility, keyedInfoSets =
                    loop (history + "d") reachProbs
                sign * utility, keyedInfoSets

                // player action
            else
                let activePlayer =
                    (Array.last rounds).Length
                        % LeducHoldem.numPlayers
                let infoSetKey =
                    sprintf "%s%s %s"
                        playerCards[activePlayer]
                        (if rounds.Length = 2 then communityCard
                         else "")
                        history
                loopNonTerminal
                    history
                    activePlayer
                    infoSetKey
                    reachProbs

        /// Recurses for non-terminal game state.
        and loopNonTerminal
            history
            activePlayer
            infoSetKey
            reachProbs =

                // get info set for current state from this player's point of view
            let actions = LeducHoldem.getLegalActions history
            let infoSet =
                getInfoSet infoSetKey infoSetMap actions.Length

                // get player's current strategy for this info set
            let strategy = InformationSet.getStrategy infoSet

                // get utility of each action
            let actionUtilities, keyedInfoSets =
                let utilities, keyedInfoSetArrays =
                    (actions, strategy.ToArray())
                        ||> Array.map2 (fun action actionProb ->
                            let reachProbs =
                                updateReachProbabilities
                                    reachProbs
                                    activePlayer
                                    actionProb
                            loop (history + action) reachProbs)
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
                            (activePlayer + 1) % LeducHoldem.numPlayers
                        reachProbs[opponent] * (actionUtilities - utility)
                    let strategy =
                        reachProbs[activePlayer] * strategy
                    InformationSet.create regrets strategy
                [|
                    yield! keyedInfoSets
                    yield infoSetKey, infoSet
                |]

            utility, keyedInfoSets

        [| 1.0; 1.0 |]
            |> DenseVector.ofArray
            |> loop ""

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
