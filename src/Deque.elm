module Deque exposing
    ( Deque
    , empty, singleton, pushFront, pushBack, append
    , popFront, popBack, left, right, dropLeft, dropRight
    , fromList, toList
    , isEmpty, member, length, first, last, equals
    , map, filter, filterMap, foldl, foldr, partition
    )

{-| A double ended queue (deque, pronounced 'deck')


## Type

@docs Deque


## Construct

@docs empty, singleton, pushFront, pushBack, append


## Deconstruct

@docs popFront, popBack, left, right, dropLeft, dropRight


## Lists

@docs fromList, toList


## Query

@docs isEmpty, member, length, first, last, equals


## Transform

@docs map, filter, filterMap, foldl, foldr, partition

-}


{-| The deque datatype

Deque equality with (==) is unreliable (equivalent deques can have a different distribution of elements between the back and the front) and should not be used.

-}
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


{-| The empty deque
-}
empty : Deque a
empty =
    Empty


{-| Check if the deque holds no elements
-}
isEmpty : Deque a -> Bool
isEmpty deque =
    case deque of
        Empty ->
            True

        _ ->
            False


{-| Create a deque consisting of a single element
-}
singleton : a -> Deque a
singleton =
    Single


{-| Adds an element to the front of the deque
-}
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

        Deque len (Four e1 e2 e3 e4) Empty (One s1) ->
            Deque (len + 1) (Three element e1 e2) Empty (Three e3 e4 s1)

        Deque len (Four e1 e2 e3 e4) middle end ->
            Deque (len + 1) (Five element e1 e2 e3 e4) middle end

        Deque len (Five e1 e2 e3 e4 e5) middle end ->
            Deque (len + 1) (Three element e1 e2) (pushBufferFront (Three e3 e4 e5) middle) end


pushBufferFront : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferFront =
    pushFront


{-| Adds an element as the last element of the deque
-}
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

        Deque len (One p1) Empty (Four e1 e2 e3 e4) ->
            Deque (len + 1) (Three p1 e1 e2) Empty (Three e3 e4 element)

        Deque len beginning middle (Four e1 e2 e3 e4) ->
            Deque (len + 1) beginning middle (Five e1 e2 e3 e4 element)

        Deque len beginning middle (Five e1 e2 e3 e4 e5) ->
            Deque (len + 1) beginning (pushBufferBack (Three e1 e2 e3) middle) (Three e4 e5 element)


pushBufferBack : Buffer a -> Deque (Buffer a) -> Deque (Buffer a)
pushBufferBack =
    pushBack


{-| Returns a tuple of the first element and the remaining elements of the deque.
The first element of the tuple will be `Nothing` íf this is run on the empty deque.
-}
popFront : Deque a -> ( Maybe a, Deque a )
popFront deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Single e1 ->
            ( Just e1, Empty )

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


{-| Returns a tuple containing the last element and the remaining elements of the deque.
The first element of the tuple will be `Nothing` if this function is run on the empty deque.
-}
popBack : Deque a -> ( Maybe a, Deque a )
popBack deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Single e1 ->
            ( Just e1, Empty )

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


{-| Take `n` number of elements from the left
-}
left : Int -> Deque a -> Deque a
left qty deque =
    let
        toDrop =
            length deque - qty
    in
    dropRight toDrop deque


{-| Take `n` number of elements from the right
-}
right : Int -> Deque a -> Deque a
right qty deque =
    let
        toDrop =
            length deque - qty
    in
    dropLeft toDrop deque


{-| Drop `n` number of elements from the left
-}
dropLeft : Int -> Deque a -> Deque a
dropLeft n deque =
    if n <= 0 then
        deque

    else
        case deque of
            Empty ->
                Empty

            Single _ ->
                if n >= 1 then
                    Empty

                else
                    deque

            Deque _ _ Empty _ ->
                case popFront deque of
                    ( Just _, rest ) ->
                        dropLeft (n - 1) rest

                    _ ->
                        deque

            Deque len prefix middle suffix ->
                let
                    prefixLength =
                        bufferLength prefix
                in
                if n > prefixLength then
                    case popFront middle of
                        ( Just newPrefix, newMiddle ) ->
                            dropLeft
                                (n - prefixLength)
                                (Deque (len - prefixLength) newPrefix newMiddle suffix)

                        ( Nothing, _ ) ->
                            deque

                else
                    case popFront deque of
                        ( Just _, rest ) ->
                            dropLeft (n - 1) rest

                        _ ->
                            deque


{-| Drop `n` number of elements from the right
-}
dropRight : Int -> Deque a -> Deque a
dropRight n deque =
    if n <= 0 then
        deque

    else
        case deque of
            Empty ->
                Empty

            Single _ ->
                if n >= 1 then
                    Empty

                else
                    deque

            Deque _ _ Empty _ ->
                case popBack deque of
                    ( Just _, rest ) ->
                        dropRight (n - 1) rest

                    _ ->
                        deque

            Deque len prefix middle suffix ->
                let
                    suffixLength =
                        bufferLength suffix
                in
                if n > suffixLength then
                    case popBack middle of
                        ( Just newSuffix, newMiddle ) ->
                            dropRight
                                (n - suffixLength)
                                (Deque (len - suffixLength) prefix newMiddle newSuffix)

                        ( Nothing, _ ) ->
                            deque

                else
                    case popBack deque of
                        ( Just _, rest ) ->
                            dropRight (n - 1) rest

                        _ ->
                            deque


{-| Check if two deques contain the same elements
-}
equals : Deque a -> Deque a -> Bool
equals dequeA dequeB =
    case ( dequeA, dequeB ) of
        ( Empty, Empty ) ->
            True

        ( Single a, Single b ) ->
            a == b

        _ ->
            if length dequeA /= length dequeB then
                False

            else
                toList dequeA == toList dequeB


{-| Converts a `List` to a deque.
-}
fromList : List a -> Deque a
fromList list =
    fromListHelper list Empty


fromListHelper : List a -> Deque a -> Deque a
fromListHelper list deque =
    case list of
        [] ->
            deque

        a :: [] ->
            fromListInsertBuffer (One a) 1 deque

        a :: b :: [] ->
            fromListInsertBuffer (Two a b) 2 deque

        a :: b :: c :: rest ->
            fromListHelper rest (fromListInsertBuffer (Three a b c) 3 deque)


fromListInsertBuffer : Buffer a -> Int -> Deque a -> Deque a
fromListInsertBuffer buffer n deque =
    case ( buffer, deque ) of
        ( One a, Empty ) ->
            Single a

        ( Two a b, Empty ) ->
            Deque n (One a) Empty (One b)

        ( Three a b c, Empty ) ->
            Deque n (Two a b) Empty (One c)

        ( Four a b c d, Empty ) ->
            Deque n (Two a b) Empty (Two c d)

        ( Five a b c d e, Empty ) ->
            Deque n (Two a b) Empty (Three c d e)

        ( _, Single a ) ->
            Deque (n + 1) (One a) Empty buffer

        ( _, Deque len beginning middle end ) ->
            Deque (len + n) beginning (pushBufferBack end middle) buffer


{-| Converts the deque to a `List`
-}
toList : Deque a -> List a
toList deque =
    foldr (\e acc -> e :: acc) [] deque


{-| Fold over the elements of the deque starting from the front.
-}
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


{-| Fold over the elements of the deque starting from the back.
-}
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


{-| Get the length of the deque
-}
length : Deque a -> Int
length deque =
    case deque of
        Empty ->
            0

        Single _ ->
            1

        Deque len _ _ _ ->
            len


bufferLength : Buffer a -> Int
bufferLength buffer =
    case buffer of
        One _ ->
            1

        Two _ _ ->
            2

        Three _ _ _ ->
            3

        Four _ _ _ _ ->
            4

        Five _ _ _ _ _ ->
            5


{-| Create a new deque containing all the elements of the provided deques. Order is preserved.
-}
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


{-| Create a new deque where every element is the result of running `fn` on every element.
-}
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


{-| Create a new deque which only contains the elements where the provided `fn` returned `True`
-}
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


{-| Allows running both `filter` and `map` in the same operation. The provided function
is run on every element and returns a `Maybe`. Only the `Just` values will be kept in the
resulting deque
-}
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


{-| Returns a tuple of deques, where the first deque contains every element where the function
returned `True`, while the second deque contains the other elements.
-}
partition : (a -> Bool) -> Deque a -> ( Deque a, Deque a )
partition pred deque =
    let
        helper element ( yays, nays ) =
            if pred element then
                ( pushBack element yays, nays )

            else
                ( yays, pushBack element nays )
    in
    foldl helper ( Empty, Empty ) deque


{-| Check if the provided element exists in this deque (using `==`).
-}
member : a -> Deque a -> Bool
member item deque =
    case popFront deque of
        ( Just frontItem, rest ) ->
            if item == frontItem then
                True

            else
                member item rest

        ( Nothing, _ ) ->
            False


{-| Get the first element of the deque
-}
first : Deque a -> Maybe a
first deque =
    case deque of
        Empty ->
            Nothing

        Single a ->
            Just a

        Deque _ (One e1) _ _ ->
            Just e1

        Deque _ (Two e1 _) _ _ ->
            Just e1

        Deque _ (Three e1 _ _) _ _ ->
            Just e1

        Deque _ (Four e1 _ _ _) _ _ ->
            Just e1

        Deque _ (Five e1 _ _ _ _) _ _ ->
            Just e1


{-| Get the last element of the deque
-}
last : Deque a -> Maybe a
last deque =
    case deque of
        Empty ->
            Nothing

        Single e1 ->
            Just e1

        Deque _ _ _ (One e1) ->
            Just e1

        Deque _ _ _ (Two _ e2) ->
            Just e2

        Deque _ _ _ (Three _ _ e3) ->
            Just e3

        Deque _ _ _ (Four _ _ _ e4) ->
            Just e4

        Deque _ _ _ (Five _ _ _ _ e5) ->
            Just e5
