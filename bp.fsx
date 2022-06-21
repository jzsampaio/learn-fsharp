// Teste realizado em <2022-06-10>

let rec _reverse (xs: int list) (acc: int list): int list=
    match xs with
        | [] -> acc
        | head::tail ->
            _reverse tail (head::acc)

let reverse xs =
    _reverse xs List.empty

reverse [1..10]

let rec _countOcurrence (xs: string list) acc =
    match xs with
        | [] -> acc
        | head::tail ->
            let newAcc =
                acc
                |> Map.change head (function
                                    | None -> Some 1
                                    | Some currentCount -> Some (currentCount + 1))
            _countOcurrence tail newAcc

let countOccurrence xs =
    _countOcurrence xs Map.empty

countOccurrence []

type Answer =
    | Balanced
    | NotBalanced

let isOpen = function | '(' | '{'| '[' -> true | _ -> false

let rec _check xs acc =
    match (xs, acc) with
        | head::tail, _ when isOpen head -> _check tail (head::acc)
        | ')'::tail, '('::accTail
        | ']'::tail, '['::accTail
        | '}'::tail, '{'::accTail -> _check tail accTail
        | [], [] -> Balanced
        | _ -> NotBalanced

let check (inputText: string) =
    _check (Seq.toList inputText) []

[ ""
  "("
  "()"
  "[]"
  "{{})"
  "[]([])"]
|> List.map (fun s -> (s, check s))
