namespace FastCfr

/// State of a game.
type GameState<'key, 'action> =

    /// Game is in progress.
    | NonTerminal of NonTerminalGameState<'key, 'action>

    /// Game is over.
    | Terminal of TerminalGameState

/// Game is in progress.
and NonTerminalGameState<'key, 'action> =
    {
        /// Index of current player.
        ActivePlayerIdx : int

        /// Unique key of this state from the active player's
        /// point of view.
        InfoSetKey : 'key

        /// Legal actions in this state.
        LegalActions : 'action[]

        /// Adds the given action to the game.
        AddAction : 'action -> GameState<'key, 'action>
    }

/// Game is over.
and TerminalGameState =
    {
        /// Per-player payoffs.
        Payoffs : float[]
    }

module TerminalGameState =

    /// Creates payoffs for each player.
    let create payoffs =
        {
            Payoffs = payoffs
        }

    /// Creates payoffs for a two-player game.
    let createTwoPlayer activePlayer payoff =
        assert(activePlayer >= 0 && activePlayer <= 1)
        let payoffs =
            if activePlayer = 0 then [| payoff; -payoff |]
            else [| -payoff; payoff |]
        create payoffs
