module DequeTests exposing (suite)

import Deque
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "Deque"
        [ describe "empty"
            [ test "isEmpty" <|
                \_ ->
                    Expect.true "isEmpty fails" (Deque.isEmpty Deque.empty)
            ]
        ]
