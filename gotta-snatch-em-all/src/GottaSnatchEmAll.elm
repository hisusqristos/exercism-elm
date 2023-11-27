module GottaSnatchEmAll exposing (..)

import List exposing (singleton)
import Set exposing (Set, intersect)


type alias Card =
    String


newCollection : Card -> Set Card
newCollection card =
    Set.singleton card


addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    Set.insert card collection
        |> Tuple.pair (Set.member card collection)


tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard yourCard theirCard collection =
    let
        isTradeable =
            (yourCard /= theirCard)
                |> (&&) (Set.member theirCard collection)
                >> not
                |> (&&) (Set.member yourCard collection)

        finalCollection =
            collection
                |> Set.remove yourCard
                |> Set.insert theirCard
    in
    ( isTradeable, finalCollection )


removeDuplicates : List Card -> List Card
removeDuplicates cards =
    cards
        |> Set.fromList
        |> Set.toList


extraCards : Set Card -> Set Card -> Int
extraCards yourCollection theirCollection =
    Set.diff yourCollection theirCollection
        |> Set.toList
        |> List.length


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
totalCards collections =
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
    allCards collections
        |> Set.size


splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards collection =
    let
        isShiny : Card -> Bool
        isShiny card =
            card
                |> String.contains "Shiny"
    in
    collection
        |> Set.partition isShiny
        |> (\( a, b ) -> ( Set.toList a, Set.toList b ))
