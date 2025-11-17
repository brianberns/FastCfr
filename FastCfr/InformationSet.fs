namespace FastCfr

open MathNet.Numerics.LinearAlgebra

/// An information set is a set of nodes in a game tree that are
/// indistinguishable for a given player. This type gathers regrets
/// and strategies for an information set.
type InformationSet =
    {
        /// Sum of regrets accumulated so far by this info set.
        RegretSum : Vector<float32>

        /// Sum of strategies accumulated so far by this info set.
        StrategySum : Vector<float32>
    }

    /// Combines the given information sets.
    static member (+)(a, b) =
        {
            RegretSum = a.RegretSum + b.RegretSum
            StrategySum = a.StrategySum + b.StrategySum
        }

module InformationSet =

    /// Creates an information set.
    let create regretSum strategySum =
        {
            RegretSum = regretSum
            StrategySum = strategySum
        }

    /// Initial info set.
    let zero numActions =
        let zero = DenseVector.zero numActions
        create zero zero

    /// Uniform strategy: All actions have equal probability.
    let private uniformStrategy numActions =
        let den = 1f / float32 numActions
        DenseVector.create numActions den

    /// Normalizes a strategy such that its elements sum to
    /// 1.0 (to represent action probabilities).
    let private normalize strategy =

            // assume no negative values during normalization
        assert(Vector.forall (fun x -> x >= 0f) strategy)

        let sum = Vector.sum strategy
        if sum > 0f then strategy / sum
        else uniformStrategy strategy.Count

    /// Computes regret-matching strategy from accumulated
    /// regrets.
    let getStrategy infoSet =
        infoSet.RegretSum
            |> Vector.map (max 0f)   // clamp negative regrets
            |> normalize

    /// Computes average strategy from accumulated strateges.
    let getAverageStrategy infoSet =
        normalize infoSet.StrategySum
