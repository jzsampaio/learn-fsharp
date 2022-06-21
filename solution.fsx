let rec fib N =
    match N with
        | 0 -> 1
        | 1 -> 1
        | x when x >= 1 -> fib(N-1) + fib(N-2)
        | _ -> failwith "N should be >= 0!"

let isFib x =
    seq { 0..x }
    |> Seq.exists (fib >> ((=) x))

let canJumpBetween (xs: int array) N a b =
    (a = -1 || xs.[a] = 1) && (b = N || xs.[b] = 1) && isFib (b - a)

// returns list of possible next states
let evolveState currentPos xs N history =
    let candidateJumps =
        [(currentPos + 1) .. N]
    candidateJumps
    |> List.filter (canJumpBetween xs N currentPos)
    |> List.map (fun x -> (x, x::history))

let rec f (states: (int * (int list)) list) xs N (solutions: int list list) : (int list) * (int list list) =
    match states with
        | (currentPosition, history) :: tail ->
            let newStates = evolveState currentPosition xs N history
            let newSolutions = if currentPosition = N then (history)::solutions else solutions
            f (newStates @ tail) xs N newSolutions
        | [] ->
            (List.empty, solutions)

let solution xs =
    let N = Array.length xs
    f [(-1, [])] xs N []

let solution2 xs =
    let _, solutions = solution xs
    solutions
    |> List.map List.length
    |> List.max

let input = [|
             0
             0
             0
             1
             1
             0
             1
             0
             0
             0
             0 |]

let ss = solution input
let answer = solution2 input
