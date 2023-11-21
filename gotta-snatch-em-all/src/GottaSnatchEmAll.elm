module GottaSnatchEmAll exposing (..)

import Set exposing (Set)


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
    Debug.todo "Please implement totalCards"


totalCards : List (Set Card) -> Int
totalCards collections =
    Debug.todo "Please implement totalCards"


splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards collection =
    Debug.todo "Please implement splitShinyCards"
