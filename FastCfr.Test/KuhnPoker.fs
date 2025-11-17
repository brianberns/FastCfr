/// Kuhn poker
module KuhnPoker

open FastCfr

/// Number of players.
let numPlayers = 2

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
    ]

/// Gets zero-based index of active player.
let getActivePlayer (history : string) =
    history.Length % numPlayers

/// Gets payoff for the active player if the game is over.
let getPayoff (cards : string[]) = function

        // opponent folds - active player wins
    | "bc" | "cbc" -> Some 1

        // showdown
    | "cc" | "bb" | "cbb" as history ->
        let payoff =
            if history.Contains('b') then 2 else 1
        let activePlayer = getActivePlayer history
        let playerCard = cards[activePlayer]
        let opponentCard =
            cards[(activePlayer + 1) % numPlayers]
        match playerCard, opponentCard with
            | "K", _
            | _, "J" -> payoff   // active player wins
            | _ -> -payoff       // opponent wins
            |> Some

        // game not over
    | _ -> None

let rec createGameState cards history =
    let activePlayer = getActivePlayer history
    match getPayoff cards history with
        | Some payoff ->
            TerminalGameState.createTwoPlayer
                activePlayer (float32 payoff)
                |> Terminal
        | None ->
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
    let rng = System.Random(0)
    let games =
        [|
            for c0 in deck do
                for c1 in deck do
                    if c0 <> c1 then
                        yield createGameState [| c0; c1 |] ""
        |]
    let gameChunks =
        Seq.initInfinite (fun _ -> rng.GetItems(games, 1))
            |> Seq.concat
            |> Seq.truncate numGames
            |> Seq.chunkBySize chunkSize
    Trainer.trainTwoPlayer gameChunks
