/// Leduc hold'em.
// https://github.com/scfenton6/leduc-cfr-poker-bot
module LeducHoldem

open FastCfr

module String =

    /// Last character in the given string, if any.
    let tryLast (str : string) =
        if str.Length = 0 then None
        else Some str[str.Length - 1]

module List =

    /// Permutes the given list.
    // http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec permutations = function
        | [] -> seq { List.empty }
        | x :: xs ->
            Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | [] -> [[x]]
        | (y :: ys) as xs ->
            (x :: xs) :: (List.map (fun x -> y :: x) (insertions x ys))

/// Number of players.
let numPlayers = 2

/// Cards in the deck.
let deck =
    [
        "J"; "J"   // Jack
        "Q"; "Q"   // Queen
        "K"; "K"   // King
    ]

/// Rank of the given card.
let rank = function
    | "J" -> 11
    | "Q" -> 12
    | "K" -> 13
    | _ -> failwith "Unexpected"

(*
* Actions:
*    x: check
*    f: fold
*    c: call
*    b: bet
*    r: raise
*    d: deal community card
*)

/// Action strings that end a round, without necessarily
/// ending the game.
let isRoundEnd = function
    | "xx"
    | "bc" | "xbc"
    | "brc" | "xbrc" -> true
    | _ -> false

/// Is the given game over?
let isTerminal rounds =
    let round = Array.last rounds
    match String.tryLast round, rounds.Length with
        | Some 'f', _ -> true
        | _, 2 -> isRoundEnd round
        | _ -> false

/// Gets legal actions for active player.
let getLegalActions history =
    match String.tryLast history with
        | None
        | Some 'd'
        | Some 'x' -> [| "x"; "b" |]
        | Some 'b' -> [| "f"; "c"; "r" |]
        | Some 'r' -> [| "f"; "c" |]
        | _ -> failwith "Unexpected"

/// Gets payoff for the active player if the game is over.
let getPayoff
    (playerCards : string[])
    communityCard
    (rounds : string[]) =

    /// Amount contributed by each player before the game
    /// starts.
    let ante = 1

    /// Payoff for the active player.
    let pay = function
        | "xx" | "bf" | "xbf" -> 0
        | "brf" | "xbrf"
        | "bc" | "xbc" -> 2
        | "brc" | "xbrc" -> 4
        | _ -> failwith "Unexpected"

    if rounds.Length = 2 then
        let pot = ante + pay rounds[0] + 2 * pay rounds[1]
        match String.tryLast rounds[1] with
            | Some 'f' -> pot
            | _ ->   // showdown
                let activePlayer = rounds[1].Length % numPlayers
                let opponent = (activePlayer + 1) % numPlayers
                if playerCards[activePlayer] = communityCard then
                    pot
                elif playerCards[opponent] = communityCard then
                    -pot
                else
                    let diff =
                        rank playerCards[activePlayer]
                            - rank playerCards[opponent]
                    if diff > 0 then pot
                    elif diff = 0 then 0
                    else -pot
    else
        assert(rounds.Length = 1)
        assert(String.tryLast rounds[0] = Some 'f')
        ante + pay rounds[0]

let rec createGameState (history : string) playerCards communityCard =
    let rounds = history.Split('d')
    let activePlayer =
        (Array.last rounds).Length % numPlayers
    if isTerminal rounds then
        let payoff = getPayoff playerCards communityCard rounds
        TerminalGameState.createTwoPlayer
            activePlayer (float32 payoff)
            |> Terminal
    elif isRoundEnd (Array.last rounds) then
        let history = history + "d"
        createGameState history playerCards communityCard
    else
        let infoSetKey =
            sprintf "%s%s %s"
                playerCards[activePlayer]
                (if rounds.Length = 2 then communityCard
                    else "")
                history
        NonTerminal {
            ActivePlayerIdx = activePlayer
            InfoSetKey = infoSetKey
            LegalActions = getLegalActions history
            AddAction =
                fun action ->
                    let history = history + action
                    createGameState history playerCards communityCard
        }

let train numGames chunkSize =
    let games =
        [|
            for cards in List.permutations deck do
                let playerCards = Seq.toArray cards[0..1]
                let communityCard = cards[2]
                yield createGameState "" playerCards communityCard
        |]
    let gameChunks =
        Seq.initInfinite (fun i -> games[i % games.Length])
            |> Seq.truncate numGames
            |> Seq.chunkBySize chunkSize
    Trainer.trainTwoPlayer gameChunks
