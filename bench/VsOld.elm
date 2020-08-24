module VsDeque exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner as Benchmark exposing (BenchmarkProgram)
import Deque as This
import OldDeque as Other


main : BenchmarkProgram
main =
    let
        sampleSize =
            999

        sampleList =
            List.repeat sampleSize 1

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
            , Benchmark.compare "range"
                "This"
                (\_ -> This.range 9 sampleSize)
                "Other"
                (\_ -> naiveRange 9 sampleSize)
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
            , Benchmark.compare "Left"
                "This"
                (\_ -> This.left (sampleSize // 2) thisDeque)
                "Other"
                (\_ -> Other.left (sampleSize // 2) otherDeque)
            , Benchmark.compare "Right"
                "This"
                (\_ -> This.right (sampleSize // 2) thisDeque)
                "Other"
                (\_ -> Other.right (sampleSize // 2) otherDeque)
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


naiveRange : Int -> Int -> Other.Deque Int
naiveRange from to =
    naiveRangeHelp from to Other.empty


naiveRangeHelp : Int -> Int -> Other.Deque Int -> Other.Deque Int
naiveRangeHelp from to acc =
    if to < from then
        acc

    else
        naiveRangeHelp (from + 1) to (Other.pushBack from acc)
