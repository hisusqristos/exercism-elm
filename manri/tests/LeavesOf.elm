module LeavesOf exposing (..)

import Expect
import Test exposing (..)
import Tree exposing (Tree(..), leaves)


suite : Test
suite =
    describe "calculate leaves of a tree"
        [ test "single noded tree is its own leaf" <|
            \() ->
                let
                    tree =
                        Tree
                            { self = 20
                            , parent = 0
                            , change = 30
                            , children = Nothing
                            }

                    leaves : List Tree
                    leaves =
                        [ tree
                        ]

                    result =
                        Tree.leaves tree
                in
                Expect.equal result leaves
        , test """ 
              a   tree with one node
              |   has an only leaf (c)
              c
            """ <|
            \() ->
                let
                    tree =
                        Tree
                            { self = 20
                            , parent = 0
                            , change = 30
                            , children =
                                Just
                                    [ Tree
                                        { self = 20
                                        , parent = 20
                                        , change = 10
                                        , children = Nothing
                                        }
                                    ]
                            }

                    leaves =
                        [ Tree
                            { self = 20
                            , parent = 20
                            , change = 10
                            , children = Nothing
                            }
                        ]

                    result =
                        Tree.leaves tree
                in
                Expect.equal result leaves
        , test """
            a
          __|__
          b   c     the leaves are (c, d, e)
        __|__
        d   e
        """ <|
            \() ->
                let
                    tree =
                        Tree
                            { self = 20
                            , parent = 0
                            , change = 30
                            , children =
                                Just
                                    [ leaf1
                                    , Tree
                                        { self = 20
                                        , parent = 20
                                        , change = 10
                                        , children = Just leaves2
                                        }
                                    ]
                            }

                    leaf1 =
                        Tree
                            { self = 20
                            , parent = 20
                            , change = 10
                            , children = Nothing
                            }

                    leaves2 =
                        [ Tree
                            { self = 80
                            , parent = 40
                            , change = 20
                            , children = Nothing
                            }
                        , Tree
                            { self = 30
                            , parent = 20
                            , change = 40
                            , children = Nothing
                            }
                        ]

                    leaves =
                        leaf1 :: leaves2

                    result =
                        Tree.leaves tree
                in
                Expect.equal result leaves
        ]
