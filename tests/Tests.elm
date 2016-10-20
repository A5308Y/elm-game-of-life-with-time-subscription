module Tests exposing (..)

import Test exposing (..)
import Expect
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
                    { cells = [ ( 1, 1 ), ( 0, 1 ) ] }
                    (updateModel { cells = [ ( 1, 2 ), ( 1, 0 ), ( 0, 1 ) ] })
        ]



--Any live cell with fewer than two live neighbours dies, as if caused by under-population.
--Any live cell with two or three live neighbours lives on to the next generation.
--Any live cell with more than three live neighbours dies, as if by over-population.
--Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
