/// Kuhn poker
module KuhnPoker3

open FastCfr

/// Number of players.
let numPlayers = 3

/// Available player actions.
let actions =
    [|
        "b"   // bet/call
        "c"   // check/fold
    |]

/// Cards in the deck.
let deck =
    [
        "J"   // Jack
        "Q"   // Queen
        "K"   // King
        "A"   // Ace
    ]

/// Value of each card.
let rank = function
    | "J" -> 0
    | "Q" -> 1
    | "K" -> 2
    | "A" -> 3
    | _ -> failwith "Unexpected"

/// Gets zero-based index of active player.
let getActivePlayer (history : string) =
    history.Length % numPlayers

/// Gets payoffs if the game is over.
let getPayoffs (cards : string[]) history =

    let calculatePayoffs (playersIn : List<_>) =
        let contributions =
            let betIndices =
                history
                    |> Seq.indexed
                    |> Seq.filter (fun (_, c) -> c = 'b')
                    |> Seq.map fst
                    |> Seq.toArray
            Array.init numPlayers (fun p ->
                let numBets =
                    betIndices
                        |> Seq.where (fun i -> i % numPlayers = p)
                        |> Seq.length
                1 + numBets)

        let pot = contributions |> Array.sum

        if playersIn.Length = 1 then // single winner
            let winner = playersIn.Head
            Array.init numPlayers (fun p ->
                (if p = winner then pot else 0) - contributions[p])
        else // showdown
            let winner = playersIn |> List.maxBy (fun p -> rank cards[p])
            Array.init numPlayers (fun p ->
                if List.contains p playersIn then
                    (if p = winner then pot else 0) - contributions[p]
                else // folded
                    -contributions[p])
        |> Some

    match history with
        // Showdowns
        | "ccc" -> calculatePayoffs [0; 1; 2]
        | "bbb" -> calculatePayoffs [0; 1; 2]
        | "cbbb" -> calculatePayoffs [0; 1; 2]
        | "ccbbb" -> calculatePayoffs [0; 1; 2]
        | "bbc" -> calculatePayoffs [0; 1]
        | "bcb" -> calculatePayoffs [0; 2]
        | "cbbc" -> calculatePayoffs [1; 2]
        | "cbcb" -> calculatePayoffs [0; 1]
        | "ccbcb" -> calculatePayoffs [1; 2]
        | "ccbbc" -> calculatePayoffs [0; 2]

        // Folds
        | "bcc" -> calculatePayoffs [0]
        | "cbcc" -> calculatePayoffs [1]
        | "ccbcc" -> calculatePayoffs [2]

        // Game is not over
        | _ -> None

let rec createGameState cards history =
    match getPayoffs cards history with
        | Some payoffs ->
            payoffs
                |> Array.map float32
                |> TerminalGameState.create
                |> Terminal
        | None ->
            let activePlayer = getActivePlayer history
            let infoSetKey =
                sprintf "%s%s" cards[activePlayer] history
            NonTerminal {
                ActivePlayerIdx = activePlayer
                InfoSetKey = infoSetKey
                LegalActions = actions
                AddAction =
                    fun action ->
                        let history = history + action
                        createGameState cards history
            }

let train numGames chunkSize =
    let games =
        [|
            for c0 in deck do
                for c1 in deck do
                    if c0 <> c1 then
                        for c2 in deck do
                            if c0 <> c2 && c1 <> c2 then
                                yield createGameState [| c0; c1; c2 |] ""
        |]
    let gameChunks =
        Seq.initInfinite (fun i -> games[i % games.Length])
            |> Seq.truncate numGames
            |> Seq.chunkBySize chunkSize
    Trainer.train 3 gameChunks