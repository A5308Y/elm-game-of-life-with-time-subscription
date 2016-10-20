module Tests exposing (..)

import Test exposing (..)
import Expect
import Set
import Cell exposing (..)


all : Test
all =
    describe "Cell"
        [ test "neighbours finds positions with difference in one coordinate" <|
            \() -> Expect.equal [ ( 1, 4 ), ( 2, 3 ) ] (neighbours [ ( 2, 3 ), ( 1, 4 ), ( 5, 5 ) ] ( 1, 3 ))
        , test "neighbours does not include istelf" <|
            \() -> Expect.equal [ ( 1, 3 ) ] (neighbours [ ( 1, 2 ), ( 1, 3 ) ] ( 1, 2 ))
        , test "update creates new cells for positions with three neighbours" <|
            \() ->
                Expect.equal
                    { cells = Set.fromList [ ( 1, 1 ), ( 0, 1 ) ] }
                    (updateModel { cells = Set.fromList [ ( 1, 2 ), ( 1, 0 ), ( 0, 1 ) ] })
        ]
