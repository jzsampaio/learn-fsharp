// https://app.codility.com/programmers/lessons/14-binary_search_algorithm/min_max_division/

// To specify 4 divisions, specify the size of each of the K.
// Sample array: [0,1,2,3,4,5]. W/ k = 3 and split (1,1,4) we have [1], [1], [2,3,4,5]
// Note that sum of elements of split = size of original array
// let s[i] be the i-th entry of the split of demension K. Note 0 <= i < k
// 0 <= s[0] <= N, N the size of the original array
// 0 <= s[1] <= N - s[0],
// 0 <= s[i] <= N - sum(s[j], 0 <= j < i)

let evalSplit S split =
    List.fold (
        fun (splitResult, consumed) batchSize ->
            let batch =  S |> (List.skip consumed >> List.take batchSize)
            (batch::splitResult, consumed + batchSize)
        ) (List.empty, 0) split
    |> (fun (xs, _) -> xs)

// TODO: how to generate all splits??
let rec _genAllSplits N K =
    // generate all tuples of k dimension
    // filter for those that match condition
    let xs = [0 .. N]
    [ 0 .. K - 1 ]
    |> List.map (fun _ -> xs)



let xs = [ 1 .. 10 ]

evalSplit xs [ 2; 0; 0; 8 ]
