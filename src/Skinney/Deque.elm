module Skinney.Deque exposing
    ( Deque
    , empty
    , filter
    , filterMap
    , foldl
    , foldr
    , fromList
    , isEmpty
    , map
    , popBack
    , popFront
    , pushBack
    , pushFront
    , singleton
    , toList
    )


type Deque a
    = Empty
    | Deque (Buffer a) (Deque (Buffer a)) (Buffer a)


type Buffer a
    = BufferEmpty
    | One a
    | Two a a
    | Three a a a
    | Four a a a a


empty : Deque a
empty =
    Empty


isEmpty : Deque a -> Bool
isEmpty deque =
    deque == empty


singleton : a -> Deque a
singleton element =
    Deque (One element) Empty BufferEmpty


pushFront : a -> Deque a -> Deque a
pushFront element deque =
    case deque of
        Empty ->
            Deque (One element) Empty BufferEmpty

        Deque BufferEmpty middle end ->
            Deque (One element) middle end

        Deque (One p1) middle end ->
            Deque (Two element p1) middle end

        Deque (Two p1 p2) middle end ->
            Deque (Three element p1 p2) middle end

        Deque (Three p1 p2 p3) middle end ->
            Deque (Four element p1 p2 p3) middle end

        Deque (Four p1 p2 p3 p4) Empty BufferEmpty ->
            Deque (Two element p1) Empty (Three p2 p3 p4)

        Deque (Four p1 p2 p3 p4) middle end ->
            Deque (Two element p1) (pushBufferFront (Three p2 p3 p4) middle) end


pushBufferFront : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferFront =
    pushFront


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Deque BufferEmpty Empty (One element)

        Deque beginning middle BufferEmpty ->
            Deque beginning middle (One element)

        Deque beginning middle (One s1) ->
            Deque beginning middle (Two s1 element)

        Deque beginning middle (Two s1 s2) ->
            Deque beginning middle (Three s1 s2 element)

        Deque beginning middle (Three s1 s2 s3) ->
            Deque beginning middle (Four s1 s2 s3 element)

        Deque BufferEmpty Empty (Four s1 s2 s3 s4) ->
            Deque (Three s1 s2 s3) Empty (Two s4 element)

        Deque beginning middle (Four s1 s2 s3 s4) ->
            Deque beginning (pushBufferBack (Three s1 s2 s3) middle) (Two s4 element)


pushBufferBack : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferBack =
    pushBack


popFront : Deque a -> ( Maybe a, Deque a )
popFront deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque (Four p1 p2 p3 p4) middle end ->
            ( Just p1, Deque (Three p2 p3 p4) middle end )

        Deque (Three p1 p2 p3) middle end ->
            ( Just p1, Deque (Two p2 p3) middle end )

        Deque (Two p1 p2) middle end ->
            ( Just p1, Deque (One p2) middle end )

        Deque (One p1) Empty BufferEmpty ->
            ( Just p1, Empty )

        Deque (One p1) middle end ->
            ( Just p1, Deque BufferEmpty middle end )

        Deque BufferEmpty Empty (One s1) ->
            ( Just s1, Empty )

        Deque BufferEmpty Empty (Two s1 s2) ->
            ( Just s1, Deque BufferEmpty Empty (One s2) )

        Deque BufferEmpty Empty (Three s1 s2 s3) ->
            ( Just s1, Deque BufferEmpty Empty (Two s2 s3) )

        Deque BufferEmpty Empty (Four s1 s2 s3 s4) ->
            ( Just s1, Deque BufferEmpty Empty (Three s2 s3 s4) )

        Deque BufferEmpty middle end ->
            let
                ( newFirst, newMiddle ) =
                    popBufferFront middle
            in
            case newFirst of
                Nothing ->
                    -- Something is seriously wrong
                    ( Nothing, Empty )

                Just val ->
                    popFront (Deque val newMiddle end)


popBufferFront : Deque (Buffer a) -> ( Maybe (Buffer a), Deque (Buffer a) )
popBufferFront =
    popFront


popBack : Deque a -> ( Maybe a, Deque a )
popBack deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque beginning middle (Four s1 s2 s3 s4) ->
            ( Just s4, Deque beginning middle (Three s1 s2 s3) )

        Deque beginning middle (Three s1 s2 s3) ->
            ( Just s3, Deque beginning middle (Two s1 s2) )

        Deque beginning middle (Two s1 s2) ->
            ( Just s2, Deque beginning middle (One s1) )

        Deque BufferEmpty Empty (One s1) ->
            ( Just s1, Empty )

        Deque beginning middle (One s1) ->
            ( Just s1, Deque beginning middle BufferEmpty )

        Deque (One p1) Empty BufferEmpty ->
            ( Just p1, Empty )

        Deque (Two p1 p2) Empty BufferEmpty ->
            ( Just p2, Deque (One p1) Empty BufferEmpty )

        Deque (Three p1 p2 p3) Empty BufferEmpty ->
            ( Just p3, Deque (Two p1 p2) Empty BufferEmpty )

        Deque (Four p1 p2 p3 p4) Empty BufferEmpty ->
            ( Just p4, Deque (Three p1 p2 p3) Empty BufferEmpty )

        Deque beginning middle BufferEmpty ->
            let
                ( newEnd, newMiddle ) =
                    popBufferBack middle
            in
            case newEnd of
                Nothing ->
                    -- Something is seriously wrong
                    ( Nothing, Empty )

                Just val ->
                    popBack (Deque beginning newMiddle val)


popBufferBack : Deque (Buffer a) -> ( Maybe (Buffer a), Deque (Buffer a) )
popBufferBack =
    popBack


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

        Deque beginning middle end ->
            bufferFoldl fn end (foldlStep (bufferFoldl fn) (bufferFoldl fn beginning acc) middle)


foldlStep : (Buffer a -> b -> b) -> b -> Deque (Buffer a) -> b
foldlStep =
    foldl


bufferFoldl : (a -> b -> b) -> Buffer a -> b -> b
bufferFoldl fn buffer acc =
    case buffer of
        BufferEmpty ->
            acc

        One a ->
            fn a acc

        Two a b ->
            fn b (fn a acc)

        Three a b c ->
            fn c (fn b (fn a acc))

        Four a b c d ->
            fn d (fn c (fn b (fn a acc)))


foldr : (a -> b -> b) -> b -> Deque a -> b
foldr fn acc deque =
    case deque of
        Empty ->
            acc

        Deque beginning middle end ->
            bufferFoldr fn beginning (foldrStep (bufferFoldr fn) (bufferFoldr fn end acc) middle)


foldrStep : (Buffer a -> b -> b) -> b -> Deque (Buffer a) -> b
foldrStep =
    foldr


bufferFoldr : (a -> b -> b) -> Buffer a -> b -> b
bufferFoldr fn buffer acc =
    case buffer of
        BufferEmpty ->
            acc

        One a ->
            fn a acc

        Two a b ->
            fn a (fn b acc)

        Three a b c ->
            fn a (fn b (fn c acc))

        Four a b c d ->
            fn a (fn b (fn c (fn d acc)))


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
