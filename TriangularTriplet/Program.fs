open System


"""
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
"""

let square x = x * x

let isRightTriangle (a, b, c) =
    square(a) + square(b) = square(c)
    || square(a) + square(c) = square(b)
    || square(c) + square (b) = square(a)

let perimeter (a, b, c) = a + b + c

let solutionsForPerimeter p =
    [
        for c in 1 .. p - 2 do
        for b in 1 .. (max (p - 1 - c) 1) do
        let a = int (sqrt ((float) (c * c - b * b))) in
        if (isRightTriangle (a, b, c)) then yield (a, b, c) ]
    |> List.filter (fun t -> (perimeter t) = p)
    |> (fun l -> (l, List.length l))

let findBestP =
    let rec loop cBestP cSolutions cLength p =
        printfn $"{cBestP} {cSolutions} {cLength} {p}"
        let (solutions, length) = solutionsForPerimeter p
        let (nBestP, nSolutions, nLength)  = if length >= cLength then (p, solutions, length) else (cBestP, cSolutions, cLength)
        if p <= 1000 then (loop nBestP nSolutions nLength (p + 1)) else (nBestP, nSolutions, nLength, -1)
    let (bestP, solutions, length, _) = loop -1 [] 0 1
    printfn $"Best P: {bestP}"
    printfn $"Solutions: {solutions}"
    printfn $"count: {length}"
    bestP

[<EntryPoint>]
let main argv =
    let p = findBestP
    printfn $"Answer: {p}"
    0
