let solution xs =
    let elements = Set.ofList xs
    let expected = Set.ofList [1 .. (List.length xs)]
    expected - elements
    |> Set.maxElement

solution [2; 3; 1; 5]
