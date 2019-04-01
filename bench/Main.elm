module Main exposing (main)

--import Deque as Folkertdev

import Benchmark exposing (Benchmark)
import Benchmark.Runner as Benchmark exposing (BenchmarkProgram)
import OldImpl as Other
import Skinney.Deque as This


main : BenchmarkProgram
main =
    let
        sampleList =
            List.repeat 100 1

        thisDeque =
            This.fromList sampleList

        otherDeque =
            Other.fromList sampleList
    in
    Benchmark.program <|
        Benchmark.describe "Deque"
            [ Benchmark.describe "Warmup - Ignore these results"
                [ Benchmark.benchmark "This"
                    (\_ -> This.fromList sampleList)
                , Benchmark.benchmark "Other"
                    (\_ -> Other.fromList sampleList)
                ]
            , Benchmark.compare "fromList"
                "This"
                (\_ -> This.fromList sampleList)
                "Other"
                (\_ -> Other.fromList sampleList)
            , Benchmark.compare "popFront"
                "This"
                (\_ -> This.popFront thisDeque)
                "Other"
                (\_ -> Other.popFront otherDeque)
            , Benchmark.compare "popBack"
                "This"
                (\_ -> This.popBack thisDeque)
                "Other"
                (\_ -> Other.popBack otherDeque)
            , Benchmark.compare "Deplete by popFront"
                "This"
                (\_ -> deplete This.popFront thisDeque)
                "Other"
                (\_ -> deplete Other.popFront otherDeque)
            , Benchmark.compare "Deplete by popBack"
                "This"
                (\_ -> deplete This.popBack thisDeque)
                "Other"
                (\_ -> deplete Other.popBack otherDeque)
            ]


deplete : (a -> ( Maybe b, a )) -> a -> a
deplete pop deque =
    case pop deque of
        ( Just _, newDeque ) ->
            deplete pop newDeque

        _ ->
            deque
