module Pangram exposing (isPangram)


containLetter : String -> Char -> Bool
containLetter str char =
    let
        letter : String
        letter =
            String.fromChar char
    in
    String.contains letter str


isPangram : String -> Bool
isPangram str =
    let
        alphabet : List Char
        alphabet =
            String.toList "abcdefghijklmnopqrstuvwxyz"

        sentence : String
        sentence =
            String.toLower str
    in
    alphabet
        |> List.all (\x -> containLetter sentence x)
