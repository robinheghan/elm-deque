module VsList exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner as Benchmark exposing (BenchmarkProgram)
import Deque as Other
import Skinney.Deque as This


main : BenchmarkProgram
main =
    let
        sampleList =
            List.repeat 99 1

        thisDeque =
            This.fromList sampleList
    in
    Benchmark.program <|
        Benchmark.describe "Deque"
            [ Benchmark.describe "Warmup - Ignore these results"
                [ Benchmark.benchmark "This"
                    (\_ -> This.fromList sampleList)
                ]
            , Benchmark.compare "pushFront"
                "This"
                (\_ -> This.pushFront 5 thisDeque)
                "Other"
                (\_ -> listPushFront 5 sampleList)
            , Benchmark.compare "Build by pushFront"
                "This"
                (\_ -> List.foldl This.pushFront thisDeque sampleList)
                "List"
                (\_ -> List.foldl listPushFront sampleList sampleList)
            , Benchmark.compare "popFront"
                "This"
                (\_ -> This.popFront thisDeque)
                "List"
                (\_ -> listPopFront sampleList)
            , Benchmark.compare "Deplete by popFront"
                "This"
                (\_ -> deplete This.popFront thisDeque)
                "List"
                (\_ -> deplete listPopFront sampleList)
            , Benchmark.compare "Map"
                "This"
                (\_ -> This.map ((*) 2) thisDeque)
                "List"
                (\_ -> List.map ((*) 2) sampleList)
            , Benchmark.compare "Filter"
                "This"
                (\_ -> This.filter (\s -> modBy 2 s == 0) thisDeque)
                "List"
                (\_ -> List.filter (\s -> modBy 2 s == 0) sampleList)
            , Benchmark.compare "Foldl"
                "This"
                (\_ -> This.foldl (+) 0 thisDeque)
                "List"
                (\_ -> List.foldl (+) 0 sampleList)
            , Benchmark.compare "Foldr"
                "This"
                (\_ -> This.foldr (+) 0 thisDeque)
                "List"
                (\_ -> List.foldr (+) 0 sampleList)
            ]


listPushFront : a -> List a -> List a
listPushFront val ls =
    val :: ls


listPopFront : List a -> ( Maybe a, List a )
listPopFront ls =
    case ls of
        [] ->
            ( Nothing, [] )

        a :: rest ->
            ( Just a, rest )


deplete : (a -> ( Maybe b, a )) -> a -> a
deplete pop deque =
    case pop deque of
        ( Just _, newDeque ) ->
            deplete pop newDeque

        _ ->
            deque
