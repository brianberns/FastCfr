namespace FastCfr

/// State of a game.
type GameState<'key, 'action, 'payoff
    when 'key : comparison
    and PayoffType<'payoff>> =

    /// Game is in progress.
    | NonTerminal of NonTerminalGameState<'key, 'action, 'payoff>

    /// Game is over.
    | Terminal of TerminalGameState<'payoff>

/// Game is in progress.
and NonTerminalGameState<'key, 'action, 'payoff
    when 'key : comparison
    and PayoffType<'payoff>> =
    {
        /// Index of current player.
        ActivePlayerIdx : int

        /// Unique key of this state from the active player's
        /// point of view.
        InfoSetKey : 'key

        /// Legal actions in this state.
        LegalActions : 'action[]

        /// Adds the given action to the game.
        AddAction : 'action -> GameState<'key, 'action, 'payoff>
    }

/// Game is over.
and TerminalGameState<'payoff when PayoffType<'payoff>> =
    {
        /// Index of payoff player.
        PayoffPlayerIdx : int

        /// Payoff for this player.
        Payoff : 'payoff
    }

module TerminalGameState =

    /// Creates a payoff from the given player's point
    /// of view.
    let inline create payoffPlayerIdx payoff =
        {
            PayoffPlayerIdx = payoffPlayerIdx
            Payoff = payoff
        }
