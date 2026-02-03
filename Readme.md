# Fast Counterfactual Regret Minimization for F#

* Supports multi-player, zero-sum games.
* Uses paralellization for speed.

Each game in a "chunk" of games is traversed on its own thread. Updates from these threads are then merged after all games in the chunk have been traversed.

Caution: This performs poorly for "imperfect recall" games, because the delayed merge prevents pruning of nodes that are revisited within a single game.
