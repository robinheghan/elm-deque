module VsDeque exposing (main)

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
            , Benchmark.compare "pushFront"
                "This"
                (\_ -> This.pushFront 5 thisDeque)
                "Other"
                (\_ -> Other.pushFront 5 otherDeque)
            , Benchmark.compare "pushBack"
                "This"
                (\_ -> This.pushBack 5 thisDeque)
                "Other"
                (\_ -> Other.pushBack 5 otherDeque)
            , Benchmark.compare "Build by pushFront"
                "This"
                (\_ -> List.foldl This.pushFront thisDeque sampleList)
                "Other"
                (\_ -> List.foldl Other.pushFront otherDeque sampleList)
            , Benchmark.compare "Build by pushBack"
                "This"
                (\_ -> List.foldl This.pushBack thisDeque sampleList)
                "Other"
                (\_ -> List.foldl Other.pushBack otherDeque sampleList)
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
            , Benchmark.compare "Map"
                "This"
                (\_ -> This.map ((*) 2) thisDeque)
                "Other"
                (\_ -> Other.map ((*) 2) otherDeque)
            , Benchmark.compare "Filter"
                "This"
                (\_ -> This.filter (\s -> modBy 2 s == 0) thisDeque)
                "Other"
                (\_ -> Other.filter (\s -> modBy 2 s == 0) otherDeque)
            , Benchmark.compare "Foldl"
                "This"
                (\_ -> This.foldl (+) 0 thisDeque)
                "Other"
                (\_ -> Other.foldl (+) 0 otherDeque)
            , Benchmark.compare "Foldr"
                "This"
                (\_ -> This.foldr (+) 0 thisDeque)
                "Other"
                (\_ -> Other.foldr (+) 0 otherDeque)
            , Benchmark.compare "Append"
                "This"
                (\_ -> This.append thisDeque thisDeque)
                "Other"
                (\_ -> Other.append otherDeque otherDeque)
            , Benchmark.compare "Length"
                "This"
                (\_ -> This.length thisDeque)
                "Other"
                (\_ -> Other.length otherDeque)
            ]


deplete : (a -> ( Maybe b, a )) -> a -> a
deplete pop deque =
    case pop deque of
        ( Just _, newDeque ) ->
            deplete pop newDeque

        _ ->
            deque
