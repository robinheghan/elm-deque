module Skinney.Deque exposing
    ( Deque
    , empty
    , filter
    , filterMap
    , foldl
    , foldr
    , fromList
    , isEmpty
    ,  map
       --, popBack
       --, popFront

    , pushBack
    , pushFront
    , singleton
    , toList
    )

import Array exposing (Array)


threshold : Int
threshold =
    4


type Deque a
    = Empty
    | Single a
    | Deque (Array a) (Deque (Array a)) (Array a)


empty : Deque a
empty =
    Empty


isEmpty : Deque a -> Bool
isEmpty deque =
    deque == empty


singleton : a -> Deque a
singleton element =
    Single element


pushFront : a -> Deque a -> Deque a
pushFront element deque =
    case deque of
        Empty ->
            Single element

        Single e1 ->
            Deque (Array.push element Array.empty) Empty (Array.push e1 Array.empty)

        Deque beginning middle end ->
            let
                asList =
                    Array.toList beginning
            in
            if Array.length beginning < threshold then
                Deque (Array.fromList (element :: asList)) middle end

            else
                Deque
                    (Array.fromList (element :: List.take 1 asList))
                    (pushArrayFront (Array.fromList (List.drop 1 asList)) middle)
                    end


pushArrayFront : Array a -> Deque (Array a) -> Deque (Array a)
pushArrayFront =
    pushFront


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Single element

        Single e1 ->
            Deque (Array.push e1 Array.empty) Empty (Array.push element Array.empty)

        Deque beginning middle end ->
            if Array.length end < threshold then
                Deque beginning middle (Array.push element end)

            else
                Deque
                    beginning
                    (pushArrayBack (Array.slice 0 -1 end) middle)
                    (Array.push element (Array.slice -1 (Array.length end) end))


pushArrayBack : Array a -> Deque (Array a) -> Deque (Array a)
pushArrayBack =
    pushBack



{- popFront : Deque a -> ( Maybe a, Deque a )
   popFront deque =
       case deque of
           Empty ->
               ( Nothing, Empty )

           Single e1 ->
               ( Just e1, Empty )

           Deque (Four e1 e2 e3 e4) middle end ->
               ( Just e1, Deque (Three e2 e3 e4) middle end )

           Deque (Three e1 e2 e3) middle end ->
               ( Just e1, Deque (Two e2 e3) middle end )

           Deque (Two e1 e2) middle end ->
               ( Just e1, Deque (One e2) middle end )

           Deque (One e1) Empty (One s1) ->
               ( Just e1, Single s1 )

           Deque (One e1) Empty (Two s1 s2) ->
               ( Just e1, Deque (One s1) Empty (One s2) )

           Deque (One e1) Empty (Three s1 s2 s3) ->
               ( Just e1, Deque (One s1) Empty (Two s2 s3) )

           Deque (One e1) Empty (Four s1 s2 s3 s4) ->
               ( Just e1, Deque (One s1) Empty (Three s2 s3 s4) )

           Deque (One e1) middle end ->
               let
                   ( newFirst, newMiddle ) =
                       popArrayFront middle
               in
               case newFirst of
                   Nothing ->
                       -- Something is seriously wrong
                       ( Nothing, Empty )

                   Just val ->
                       ( Just e1, Deque val newMiddle end )


   popArrayFront : Deque (Array a) -> ( Maybe (Array a), Deque (Array a) )
   popArrayFront =
       popFront


   popBack : Deque a -> ( Maybe a, Deque a )
   popBack deque =
       case deque of
           Empty ->
               ( Nothing, Empty )

           Single e1 ->
               ( Just e1, Empty )

           Deque beginning middle (Four e1 e2 e3 e4) ->
               ( Just e4, Deque beginning middle (Three e1 e2 e3) )

           Deque beginning middle (Three e1 e2 e3) ->
               ( Just e3, Deque beginning middle (Two e1 e2) )

           Deque beginning middle (Two e1 e2) ->
               ( Just e2, Deque beginning middle (One e1) )

           Deque (One p1) Empty (One e1) ->
               ( Just e1, Single p1 )

           Deque (Two p1 p2) Empty (One e1) ->
               ( Just e1, Deque (One p1) Empty (One p2) )

           Deque (Three p1 p2 p3) Empty (One e1) ->
               ( Just e1, Deque (Two p1 p2) Empty (One p3) )

           Deque (Four p1 p2 p3 p4) Empty (One e1) ->
               ( Just e1, Deque (Three p1 p2 p3) Empty (One p4) )

           Deque beginning middle (One e1) ->
               let
                   ( newEnd, newMiddle ) =
                       popArrayBack middle
               in
               case newEnd of
                   Nothing ->
                       -- Something is seriously wrong
                       ( Nothing, Empty )

                   Just val ->
                       ( Just e1, Deque beginning newMiddle val )


   popArrayBack : Deque (Array a) -> ( Maybe (Array a), Deque (Array a) )
   popArrayBack =
       popBack

-}


fromList : List a -> Deque a
fromList list =
    List.foldl pushBack empty list


toList : Deque a -> List a
toList deque =
    foldr (::) [] deque


foldl : (a -> b -> b) -> b -> Deque a -> b
foldl fn acc deque =
    case deque of
        Empty ->
            acc

        Single a ->
            fn a acc

        Deque beginning middle end ->
            bufferFoldl fn end (foldlStep (bufferFoldl fn) (bufferFoldl fn beginning acc) middle)


foldlStep : (Array a -> b -> b) -> b -> Deque (Array a) -> b
foldlStep =
    foldl


bufferFoldl : (a -> b -> b) -> Array a -> b -> b
bufferFoldl fn buffer acc =
    Array.foldl fn acc buffer


foldr : (a -> b -> b) -> b -> Deque a -> b
foldr fn acc deque =
    case deque of
        Empty ->
            acc

        Single a ->
            fn a acc

        Deque beginning middle end ->
            bufferFoldr fn beginning (foldrStep (bufferFoldr fn) (bufferFoldr fn end acc) middle)


foldrStep : (Array a -> b -> b) -> b -> Deque (Array a) -> b
foldrStep =
    foldr


bufferFoldr : (a -> b -> b) -> Array a -> b -> b
bufferFoldr fn buffer acc =
    Array.foldr fn acc buffer


map : (a -> b) -> Deque a -> Deque b
map fn deque =
    let
        helper element acc =
            pushBack (fn element) acc
    in
    foldl helper empty deque


filter : (a -> Bool) -> Deque a -> Deque a
filter fn deque =
    let
        helper element acc =
            if fn element then
                pushBack element acc

            else
                acc
    in
    foldl helper empty deque


filterMap : (a -> Maybe b) -> Deque a -> Deque b
filterMap fn deque =
    let
        helper a acc =
            case fn a of
                Just b ->
                    pushBack b acc

                Nothing ->
                    acc
    in
    foldl helper empty deque
