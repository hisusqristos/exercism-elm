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
            let
                egg =
                    case remainderBy 2 n of
                        1 ->
                            [ 1 ]

                        _ ->
                            []
            in
            egg ++ noZeroBinary (n // 2)
