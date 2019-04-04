module CurrImpl exposing
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
    | Single a
    | Deque (Buffer a) (Deque (Buffer a)) (Buffer a)


type Buffer a
    = One a
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
    Single element


pushFront : a -> Deque a -> Deque a
pushFront element deque =
    case deque of
        Empty ->
            Single element

        Single e1 ->
            Deque (One element) Empty (One e1)

        Deque (One e1) middle end ->
            Deque (Two element e1) middle end

        Deque (Two e1 e2) middle end ->
            Deque (Three element e1 e2) middle end

        Deque (Three e1 e2 e3) middle end ->
            Deque (Four element e1 e2 e3) middle end

        Deque (Four e1 e2 e3 e4) Empty (One s1) ->
            Deque (Two element e1) Empty (Four e2 e3 e4 s1)

        Deque (Four e1 e2 e3 e4) middle end ->
            Deque (Two element e1) (pushBufferFront (Three e2 e3 e4) middle) end


pushBufferFront : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferFront =
    pushFront


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Single element

        Single e1 ->
            Deque (One e1) Empty (One element)

        Deque beginning middle (One e1) ->
            Deque beginning middle (Two e1 element)

        Deque beginning middle (Two e1 e2) ->
            Deque beginning middle (Three e1 e2 element)

        Deque beginning middle (Three e1 e2 e3) ->
            Deque beginning middle (Four e1 e2 e3 element)

        Deque (One p1) Empty (Four e1 e2 e3 e4) ->
            Deque (Four p1 e1 e2 e3) Empty (Two e4 element)

        Deque beginning middle (Four e1 e2 e3 e4) ->
            Deque beginning (pushBufferBack (Three e1 e2 e3) middle) (Two e4 element)


pushBufferBack : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferBack =
    pushBack


popFront : Deque a -> ( Maybe a, Deque a )
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
                    popBufferFront middle
            in
            case newFirst of
                Nothing ->
                    -- Something is seriously wrong
                    ( Nothing, Empty )

                Just val ->
                    ( Just e1, Deque val newMiddle end )


popBufferFront : Deque (Buffer a) -> ( Maybe (Buffer a), Deque (Buffer a) )
popBufferFront =
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
                    popBufferBack middle
            in
            case newEnd of
                Nothing ->
                    -- Something is seriously wrong
                    ( Nothing, Empty )

                Just val ->
                    ( Just e1, Deque beginning newMiddle val )


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

        Single a ->
            fn a acc

        Deque beginning middle end ->
            bufferFoldl fn end (foldlStep (bufferFoldl fn) (bufferFoldl fn beginning acc) middle)


foldlStep : (Buffer a -> b -> b) -> b -> Deque (Buffer a) -> b
foldlStep =
    foldl


bufferFoldl : (a -> b -> b) -> Buffer a -> b -> b
bufferFoldl fn buffer acc =
    case buffer of
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

        Single a ->
            fn a acc

        Deque beginning middle end ->
            bufferFoldr fn beginning (foldrStep (bufferFoldr fn) (bufferFoldr fn end acc) middle)


foldrStep : (Buffer a -> b -> b) -> b -> Deque (Buffer a) -> b
foldrStep =
    foldr


bufferFoldr : (a -> b -> b) -> Buffer a -> b -> b
bufferFoldr fn buffer acc =
    case buffer of
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
