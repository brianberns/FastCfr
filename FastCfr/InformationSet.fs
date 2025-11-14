namespace FastCfr

open System
open MathNet.Numerics.LinearAlgebra

/// Numeric type that represents utility.
type PayoffType<'t
    when 't : (static member Zero : 't)
    and 't : (static member One : 't)
    and 't : (static member DivideByInt : 't * int -> 't)
    and 't : (static member (+) : 't * 't -> 't)
    and 't : (static member (-) : 't * 't -> 't)
    and 't : (static member (*) : 't * 't -> 't)
    and 't : (static member (~-) : 't -> 't)
    and 't : (static member op_Explicit : 't -> float)
    and 't : comparison
    and 't: (new: unit -> 't)
    and 't: struct
    and 't :> ValueType
    and 't :> IEquatable<'t>
    and 't :> IFormattable> = 't

/// An information set is a set of nodes in a game tree that are
/// indistinguishable for a given player. This type gathers regrets
/// and strategies for an information set.
type InformationSet<'t when PayoffType<'t>> =
    {
        /// Sum of regrets accumulated so far by this info set.
        RegretSum : Vector<'t>

        /// Sum of strategies accumulated so far by this info set.
        StrategySum : Vector<'t>

        /// Number of visits to this information set so far. This
        /// is purely informational.
        NumVisits : int
    }

    /// Combines the given information sets.
    static member inline (+)(a, b) =
        {
            RegretSum = a.RegretSum + b.RegretSum
            StrategySum = a.StrategySum + b.StrategySum
            NumVisits = a.NumVisits + b.NumVisits
        }

module InformationSet =

    /// Creates an information set.
    let inline create regretSum strategySum =
        {
            RegretSum = regretSum
            StrategySum = strategySum
            NumVisits = 1
        }

    /// Initial info set.
    let inline zero numActions =
        let zero = DenseVector.zero numActions
        create zero zero

    /// Uniform strategy: All actions have equal probability.
    let inline private uniformStrategy<'t when PayoffType<'t>>
        numActions =
        let den = 't.DivideByInt('t.One, numActions)
        DenseVector.create numActions den

    /// Normalizes a strategy such that its elements sum to
    /// 1.0 (to represent action probabilities).
    let inline private normalize<'t when PayoffType<'t>>
        (strategy : Vector<'t>) =

            // assume no negative values during normalization
        assert(Vector.forall (fun x -> x >= 't.Zero) strategy)

        let sum = Vector.sum strategy
        if sum > 't.Zero then Vector.(/)(strategy, sum)   // `strategy / sum` doesn't work?
        else uniformStrategy strategy.Count

    /// Computes regret-matching strategy from accumulated
    /// regrets.
    let inline getStrategy<'t when PayoffType<'t>> infoSet =
        infoSet.RegretSum
            |> Vector.map (max 't.Zero)   // clamp negative regrets
            |> normalize

    /// Computes average strategy from accumulated strateges.
    let inline getAverageStrategy infoSet =
        normalize infoSet.StrategySum
