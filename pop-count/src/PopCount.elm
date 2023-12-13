module PopCount exposing (eggCount)


eggCount : Int -> Int
eggCount =
    toBinary >> List.sum


toBinary : Int -> List Int
toBinary n =
    case n of
        0 ->
            []

        1 ->
            [ 1 ]

        _ ->
            remainderBy 2 n :: toBinary (n // 2)
