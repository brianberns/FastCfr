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
        /// Index of payoff player.
        PayoffPlayerIdx : int

        /// Payoff for this player.
        Payoff : float32
    }

module TerminalGameState =

    /// Creates a payoff from the given player's point
    /// of view.
    let create payoffPlayerIdx payoff =
        {
            PayoffPlayerIdx = payoffPlayerIdx
            Payoff = payoff
        }