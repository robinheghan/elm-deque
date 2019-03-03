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
        , describe "singleton"
            [ test "Creates deque of single element" <|
                \_ ->
                    let
                        useOfSingleton =
                            Deque.singleton 1

                        useOfPush =
                            Deque.pushFront 1 Deque.empty
                    in
                    Expect.equal useOfSingleton useOfPush
            ]
        , describe "toList"
            [ test "Empty deque gives empty list" <|
                \_ ->
                    Expect.equalLists [] (Deque.toList Deque.empty)
            , fuzz (Fuzz.list Fuzz.int) "pushFront is the same as List :: operator" <|
                \list ->
                    List.foldl Deque.pushFront Deque.empty list
                        |> Deque.toList
                        |> Expect.equalLists (List.reverse list)
            , fuzz (Fuzz.list Fuzz.int) "pushBack should result in the same List" <|
                \list ->
                    List.foldl Deque.pushBack Deque.empty list
                        |> Deque.toList
                        |> Expect.equalLists list
            , fuzz (Fuzz.list Fuzz.int) "fromList works as the inverse of toList" <|
                \list ->
                    Deque.fromList list
                        |> Deque.toList
                        |> Expect.equalLists list
            ]
        ]
