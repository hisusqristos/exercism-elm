module GottaSnatchEmAll exposing (..)

import List
import Set exposing (Set)


type alias Card =
    String


newCollection : Card -> Set Card
newCollection =
    Set.singleton


addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    ( Set.member card collection, Set.insert card collection )


tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard yourCard theirCard collection =
    let
        isTradeable =
            (&&) (not (Set.member theirCard collection))
                << (&&) (Set.member yourCard collection)
                << (/=) theirCard

        final =
            Set.remove yourCard >> Set.insert theirCard
    in
    ( isTradeable yourCard, final collection )


removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList


extraCards : Set Card -> Set Card -> Int
extraCards yourCollection =
    Set.diff yourCollection
        >> Set.toList
        >> List.length


boringCards : List (Set Card) -> List Card
boringCards collections =
    case collections of
        [] ->
            []

        firstClt :: secondClt :: rest ->
            Set.intersect firstClt secondClt
                |> (\set -> boringCards (set :: rest))

        [ singleClt ] ->
            Set.toList singleClt


totalCards : List (Set Card) -> Int
totalCards =
    let
        allCards : List (Set Card) -> Set Card
        allCards cardSets =
            case cardSets of
                [] ->
                    Set.empty

                first :: rest ->
                    first
                        |> Set.union (allCards rest)
    in
    Set.size << allCards


splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards =
    let
        isShiny : Card -> Bool
        isShiny =
            String.contains "Shiny"
    in
    Set.partition isShiny
        >> Tuple.mapBoth Set.toList Set.toList
