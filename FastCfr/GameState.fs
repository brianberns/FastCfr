namespace FastCfr

type GameState<'action> =

    /// Game is in progress.
    | NonTerminal of NonTerminalGameState<'action>

    /// Game is over.
    | Terminal of TerminalGameState

/// Game is in progress.
and NonTerminalGameState<'action> =
    {
        /// Legal actions in this state.
        LegalActions : 'action[]

        /// Unique key of this state.
        InfoSetKey : string

        /// Index of current player.
        ActivePlayerIdx : int

        /// Adds the given action to the game.
        AddAction : 'action -> GameState<'action>
    }

/// Game is over.
and TerminalGameState =
    {
        /// Index of payoff player.
        PayoffPlayerIdx : int

        /// Payoff for this player.
        Payoff : float
    }

module TerminalGameState =

    /// Creates a payoff from the given player's point
    /// of view.
    let create payoffPlayerIdx payoff =
        {
            PayoffPlayerIdx = payoffPlayerIdx
            Payoff = payoff
        }