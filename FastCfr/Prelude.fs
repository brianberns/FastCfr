namespace FastCfr

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
