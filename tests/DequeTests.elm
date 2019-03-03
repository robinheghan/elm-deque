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
            , test "fromList of empty list create the empty deque" <|
                \_ ->
                    Expect.equal Deque.empty (Deque.fromList [])
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
            [ fuzz (Fuzz.list Fuzz.int) "fromList works as the inverse of toList" <|
                \list ->
                    Deque.fromList list
                        |> Deque.toList
                        |> Expect.equalLists list
            ]
        , describe "Push"
            [ fuzz (Fuzz.list Fuzz.int) "pushFront is the same as List :: operator" <|
                \list ->
                    List.foldl Deque.pushFront Deque.empty list
                        |> Deque.toList
                        |> Expect.equalLists (List.reverse list)
            , fuzz (Fuzz.list Fuzz.int) "pushBack should result in the same List" <|
                \list ->
                    List.foldl Deque.pushBack Deque.empty list
                        |> Deque.toList
                        |> Expect.equalLists list
            ]
        , describe "Pop"
            [ fuzz (Fuzz.list Fuzz.int) "popFront" <|
                \list ->
                    let
                        alternate addFront addBack =
                            \el ( toBack, acc ) ->
                                if toBack then
                                    ( False, addBack el acc )

                                else
                                    ( True, addFront el acc )

                        listAddBack a acc =
                            acc ++ [ a ]

                        alternatedList =
                            List.foldl (alternate (::) listAddBack) ( True, [] ) list
                                |> Tuple.second

                        listResult =
                            case alternatedList of
                                [] ->
                                    Nothing

                                x :: xs ->
                                    Just ( x, xs )
                    in
                    List.foldl (alternate Deque.pushFront Deque.pushBack) ( True, Deque.empty ) list
                        |> Tuple.second
                        |> Deque.popFront
                        |> Maybe.map (Tuple.mapSecond Deque.toList)
                        |> Expect.equal listResult
            , fuzz (Fuzz.list Fuzz.int) "popBack" <|
                \list ->
                    let
                        alternate addFront addBack =
                            \el ( toBack, acc ) ->
                                if toBack then
                                    ( False, addBack el acc )

                                else
                                    ( True, addFront el acc )

                        listAddBack a acc =
                            acc ++ [ a ]

                        alternatedList =
                            List.foldl (alternate (::) listAddBack) ( True, [] ) list
                                |> Tuple.second

                        listResult =
                            case List.reverse alternatedList of
                                [] ->
                                    Nothing

                                x :: xs ->
                                    Just ( x, xs )
                    in
                    List.foldl (alternate Deque.pushFront Deque.pushBack) ( True, Deque.empty ) list
                        |> Tuple.second
                        |> Deque.popBack
                        |> Maybe.map (Tuple.mapSecond (List.reverse << Deque.toList))
                        |> Expect.equal listResult
            ]
        , describe "Conversions"
            [ fuzz (Fuzz.list Fuzz.string) "foldl works like List.foldl" <|
                \list ->
                    let
                        listResult =
                            List.foldl (++) "" list

                        dequeResult =
                            Deque.foldl (++) "" (Deque.fromList list)
                    in
                    Expect.equal listResult dequeResult
            , fuzz (Fuzz.list Fuzz.string) "foldr works like List.foldr" <|
                \list ->
                    let
                        listResult =
                            List.foldr (++) "" list

                        dequeResult =
                            Deque.foldr (++) "" (Deque.fromList list)
                    in
                    Expect.equal listResult dequeResult
            , fuzz (Fuzz.list Fuzz.int) "map works like List.map" <|
                \list ->
                    let
                        listResult =
                            List.map ((+) 1) list

                        dequeResult =
                            Deque.map ((+) 1) (Deque.fromList list)
                    in
                    Expect.equalLists listResult (Deque.toList dequeResult)
            , fuzz (Fuzz.list Fuzz.int) "filter works like List.filter" <|
                \list ->
                    let
                        listResult =
                            List.filter (\n -> modBy 2 n == 0) list

                        dequeResult =
                            Deque.filter (\n -> modBy 2 n == 0) (Deque.fromList list)
                    in
                    Expect.equalLists listResult (Deque.toList dequeResult)
            , fuzz (Fuzz.list Fuzz.int) "filterMap works like List.filterMap" <|
                \list ->
                    let
                        fn n =
                            if modBy 2 n == 0 then
                                Just 1

                            else
                                Nothing

                        listResult =
                            List.filterMap fn list

                        dequeResult =
                            Deque.filterMap fn (Deque.fromList list)
                    in
                    Expect.equalLists listResult (Deque.toList dequeResult)
            ]
        ]
