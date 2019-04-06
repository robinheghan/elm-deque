module Skinney.Deque exposing
    ( Deque
    , append
    , empty
    , filter
    , filterMap
    , foldl
    , foldr
    , fromList
    , isEmpty
    , length
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
    | Deque Int (Buffer a) (Deque (Buffer a)) (Buffer a)


type Buffer a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
    | Five a a a a a
    | Six a a a a a a


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
            Deque 2 (One element) Empty (One e1)

        Deque len (One e1) middle end ->
            Deque (len + 1) (Two element e1) middle end

        Deque len (Two e1 e2) middle end ->
            Deque (len + 1) (Three element e1 e2) middle end

        Deque len (Three e1 e2 e3) middle end ->
            Deque (len + 1) (Four element e1 e2 e3) middle end

        Deque len (Four e1 e2 e3 e4) middle end ->
            Deque (len + 1) (Five element e1 e2 e3 e4) middle end

        Deque len (Five e1 e2 e3 e4 e5) middle end ->
            Deque (len + 1) (Six element e1 e2 e3 e4 e5) middle end

        Deque len (Six e1 e2 e3 e4 e5 e6) Empty (One s1) ->
            Deque (len + 1) (Four element e1 e2 e3) Empty (Four e4 e5 e6 s1)

        Deque len (Six e1 e2 e3 e4 e5 e6) middle end ->
            Deque (len + 1) (Two element e1) (pushBufferFront (Five e2 e3 e4 e5 e6) middle) end


pushBufferFront : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferFront =
    pushFront


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Single element

        Single e1 ->
            Deque 2 (One e1) Empty (One element)

        Deque len beginning middle (One e1) ->
            Deque (len + 1) beginning middle (Two e1 element)

        Deque len beginning middle (Two e1 e2) ->
            Deque (len + 1) beginning middle (Three e1 e2 element)

        Deque len beginning middle (Three e1 e2 e3) ->
            Deque (len + 1) beginning middle (Four e1 e2 e3 element)

        Deque len beginning middle (Four e1 e2 e3 e4) ->
            Deque (len + 1) beginning middle (Five e1 e2 e3 e4 element)

        Deque len beginning middle (Five e1 e2 e3 e4 e5) ->
            Deque (len + 1) beginning middle (Six e1 e2 e3 e4 e5 element)

        Deque len (One p1) Empty (Six e1 e2 e3 e4 e5 e6) ->
            Deque (len + 1) (Four p1 e1 e2 e3) Empty (Four e4 e5 e6 element)

        Deque len beginning middle (Six e1 e2 e3 e4 e5 e6) ->
            Deque (len + 1) beginning (pushBufferBack (Five e1 e2 e3 e4 e5) middle) (Two e6 element)


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

        Deque len (Six e1 e2 e3 e4 e5 e6) middle end ->
            ( Just e1, Deque (len - 1) (Five e2 e3 e4 e5 e6) middle end )

        Deque len (Five e1 e2 e3 e4 e5) middle end ->
            ( Just e1, Deque (len - 1) (Four e2 e3 e4 e5) middle end )

        Deque len (Four e1 e2 e3 e4) middle end ->
            ( Just e1, Deque (len - 1) (Three e2 e3 e4) middle end )

        Deque len (Three e1 e2 e3) middle end ->
            ( Just e1, Deque (len - 1) (Two e2 e3) middle end )

        Deque len (Two e1 e2) middle end ->
            ( Just e1, Deque (len - 1) (One e2) middle end )

        Deque len (One e1) Empty (One s1) ->
            ( Just e1, Single s1 )

        Deque len (One e1) Empty (Two s1 s2) ->
            ( Just e1, Deque (len - 1) (One s1) Empty (One s2) )

        Deque len (One e1) Empty (Three s1 s2 s3) ->
            ( Just e1, Deque (len - 1) (One s1) Empty (Two s2 s3) )

        Deque len (One e1) Empty (Four s1 s2 s3 s4) ->
            ( Just e1, Deque (len - 1) (Two s1 s2) Empty (Two s3 s4) )

        Deque len (One e1) Empty (Five s1 s2 s3 s4 s5) ->
            ( Just e1, Deque (len - 1) (One s1) Empty (Four s2 s3 s4 s5) )

        Deque len (One e1) Empty (Six s1 s2 s3 s4 s5 s6) ->
            ( Just e1, Deque (len - 1) (Three s1 s2 s3) Empty (Three s4 s5 s6) )

        Deque len (One e1) middle end ->
            let
                ( newFirst, newMiddle ) =
                    popBufferFront middle
            in
            case newFirst of
                Nothing ->
                    -- Something is seriously wrong
                    ( Nothing, Empty )

                Just val ->
                    ( Just e1, Deque (len - 1) val newMiddle end )


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

        Deque len beginning middle (Six e1 e2 e3 e4 e5 e6) ->
            ( Just e6, Deque (len - 1) beginning middle (Five e1 e2 e3 e4 e5) )

        Deque len beginning middle (Five e1 e2 e3 e4 e5) ->
            ( Just e5, Deque (len - 1) beginning middle (Four e1 e2 e3 e4) )

        Deque len beginning middle (Four e1 e2 e3 e4) ->
            ( Just e4, Deque (len - 1) beginning middle (Three e1 e2 e3) )

        Deque len beginning middle (Three e1 e2 e3) ->
            ( Just e3, Deque (len - 1) beginning middle (Two e1 e2) )

        Deque len beginning middle (Two e1 e2) ->
            ( Just e2, Deque (len - 1) beginning middle (One e1) )

        Deque len (One p1) Empty (One e1) ->
            ( Just e1, Single p1 )

        Deque len (Two p1 p2) Empty (One e1) ->
            ( Just e1, Deque (len - 1) (One p1) Empty (One p2) )

        Deque len (Three p1 p2 p3) Empty (One e1) ->
            ( Just e1, Deque (len - 1) (Two p1 p2) Empty (One p3) )

        Deque len (Four p1 p2 p3 p4) Empty (One e1) ->
            ( Just e1, Deque (len - 1) (Two p1 p2) Empty (Two p3 p4) )

        Deque len (Five p1 p2 p3 p4 p5) Empty (One e1) ->
            ( Just e1, Deque (len - 1) (Four p1 p2 p3 p4) Empty (One p5) )

        Deque len (Six p1 p2 p3 p4 p5 p6) Empty (One e1) ->
            ( Just e1, Deque (len - 1) (Three p1 p2 p3) Empty (Three p4 p5 p6) )

        Deque len beginning middle (One e1) ->
            let
                ( newEnd, newMiddle ) =
                    popBufferBack middle
            in
            case newEnd of
                Nothing ->
                    -- Something is seriously wrong
                    ( Nothing, Empty )

                Just val ->
                    ( Just e1, Deque (len - 1) beginning newMiddle val )


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

        Deque _ beginning middle end ->
            bufferFoldl fn end (foldlStep (\b a -> bufferFoldl fn b a) (bufferFoldl fn beginning acc) middle)


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

        Five a b c d e ->
            fn e (fn d (fn c (fn b (fn a acc))))

        Six a b c d e f ->
            fn f (fn e (fn d (fn c (fn b (fn a acc)))))


foldr : (a -> b -> b) -> b -> Deque a -> b
foldr fn acc deque =
    case deque of
        Empty ->
            acc

        Single a ->
            fn a acc

        Deque _ beginning middle end ->
            bufferFoldr fn beginning (foldrStep (\b a -> bufferFoldr fn b a) (bufferFoldr fn end acc) middle)


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

        Five a b c d e ->
            fn a (fn b (fn c (fn d (fn e acc))))

        Six a b c d e f ->
            fn a (fn b (fn c (fn d (fn e (fn f acc)))))


length : Deque a -> Int
length deque =
    case deque of
        Empty ->
            0

        Single _ ->
            1

        Deque len _ _ _ ->
            len


append : Deque a -> Deque a -> Deque a
append dequeA dequeB =
    case ( dequeA, dequeB ) of
        ( Empty, _ ) ->
            dequeB

        ( _, Empty ) ->
            dequeA

        ( Single e1, _ ) ->
            pushFront e1 dequeB

        ( _, Single e1 ) ->
            pushBack e1 dequeA

        ( Deque l1 b1 m1 e1, Deque l2 b2 m2 e2 ) ->
            let
                newMiddle =
                    appendStep
                        (pushBufferBack e1 m1)
                        (pushBufferFront b2 m2)
            in
            Deque (l1 + l2) b1 newMiddle e2


appendStep : Deque (Buffer a) -> Deque (Buffer a) -> Deque (Buffer a)
appendStep =
    append


map : (a -> b) -> Deque a -> Deque b
map fn deque =
    case deque of
        Empty ->
            Empty

        Single a ->
            Single (fn a)

        Deque len beginning middle end ->
            Deque len
                (bufferMap fn beginning)
                (mapStep (\buf -> bufferMap fn buf) middle)
                (bufferMap fn end)


mapStep : (Buffer a -> Buffer b) -> Deque (Buffer a) -> Deque (Buffer b)
mapStep =
    map


bufferMap : (a -> b) -> Buffer a -> Buffer b
bufferMap fn buffer =
    case buffer of
        One a ->
            One (fn a)

        Two a b ->
            Two (fn a) (fn b)

        Three a b c ->
            Three (fn a) (fn b) (fn c)

        Four a b c d ->
            Four (fn a) (fn b) (fn c) (fn d)

        Five a b c d e ->
            Five (fn a) (fn b) (fn c) (fn d) (fn e)

        Six a b c d e f ->
            Six (fn a) (fn b) (fn c) (fn d) (fn e) (fn f)


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
