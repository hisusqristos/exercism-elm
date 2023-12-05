module PopCount exposing (eggCount)


eggCount : Int -> Int
eggCount =
    noZeroBinary >> List.length


noZeroBinary : Int -> List Int
noZeroBinary n =
    case n of
        0 ->
            []

        1 ->
            [ 1 ]

        _ ->
            remainderBy 2 n :: noZeroBinary (n // 2)
