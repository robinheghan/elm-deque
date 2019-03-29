module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner as Benchmark exposing (BenchmarkProgram)
import Deque as Folkertdev
import Skinney.Deque as Skinney


main : BenchmarkProgram
main =
    let
        sampleList =
            List.repeat 100 1

        skinneyDeque =
            Skinney.fromList sampleList

        folkertdevDeque =
            Folkertdev.fromList sampleList
    in
    Benchmark.program <|
        Benchmark.describe "Deque"
            [ Benchmark.describe "Warmup - Ignore these results"
                [ Benchmark.benchmark "Skinney"
                    (\_ -> Skinney.fromList sampleList)
                , Benchmark.benchmark "Folkertdev"
                    (\_ -> Folkertdev.fromList sampleList)
                ]
            , Benchmark.compare "fromList"
                "Skinney"
                (\_ -> Skinney.fromList sampleList)
                "folkertdev"
                (\_ -> Folkertdev.fromList sampleList)
            , Benchmark.compare "popFront"
                "Skinney"
                (\_ -> Skinney.popFront skinneyDeque)
                "folkertdev"
                (\_ -> Folkertdev.popFront folkertdevDeque)
            , Benchmark.compare "popBack"
                "Skinney"
                (\_ -> Skinney.popBack skinneyDeque)
                "folkertdev"
                (\_ -> Folkertdev.popBack folkertdevDeque)
            , Benchmark.compare "Deplete by popFront"
                "Skinney"
                (\_ -> deplete Skinney.popFront skinneyDeque)
                "folkertdev"
                (\_ -> deplete Folkertdev.popFront folkertdevDeque)
            , Benchmark.compare "Deplete by popBack"
                "Skinney"
                (\_ -> deplete Skinney.popBack skinneyDeque)
                "folkertdev"
                (\_ -> deplete Folkertdev.popBack folkertdevDeque)
            ]


deplete : (a -> ( Maybe b, a )) -> a -> a
deplete pop deque =
    case pop deque of
        ( Just _, newDeque ) ->
            deplete pop newDeque

        _ ->
            deque
