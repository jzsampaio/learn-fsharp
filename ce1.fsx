// Part I

let strToInt (str: string) =
    try
        int(str)
        |> Some
    with
        | :? System.FormatException -> None

type MyWorkflowBuilder() =
    member this.Bind(m, f) =
        match m with
        | Some x -> f x
        | None -> None

    member this.Return(x) = Some x

let flow = MyWorkflowBuilder()

let stringAddWorkflow x y z = flow {
    let! a = strToInt x
    let! b = strToInt y
    let! c = strToInt z
    return a + b + c
    }

let good = stringAddWorkflow "1" "10" "100"
let bad = stringAddWorkflow "1" "xyz" "100"

// Part II

let strAdd str i =
    match strToInt str with
        | Some j -> Some (i + j)
        | None -> None

let (>>=) m f =
    match m with
        | Some x -> f x
        | None -> None

let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let bad = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"
