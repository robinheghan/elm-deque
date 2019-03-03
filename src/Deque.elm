module Deque exposing
    ( Deque
    , empty
    , fromList
    , isEmpty
    , pushBack
    , pushFront
    , singleton
    , toList
    )


type Deque a
    = Deque (Buffer a) (Deque a) (Buffer a)
    | EmptyDeque


type Buffer a
    = EmptyBuffer
    | One a
    | Two a a
    | Three a a a
    | Four a a a a


empty : Deque a
empty =
    EmptyDeque


isEmpty : Deque a -> Bool
isEmpty deque =
    deque == empty


singleton : a -> Deque a
singleton element =
    Deque (One element) EmptyDeque EmptyBuffer


pushFront : a -> Deque a -> Deque a
pushFront element deque =
    case deque of
        EmptyDeque ->
            Deque (One element) EmptyDeque EmptyBuffer

        nextMiddle ->
            Deque (One element) nextMiddle EmptyBuffer


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        EmptyDeque ->
            Deque EmptyBuffer EmptyDeque (One element)

        nextMiddle ->
            Deque EmptyBuffer nextMiddle (One element)


fromList : List a -> Deque a
fromList list =
    List.foldl pushBack empty list


toList : Deque a -> List a
toList deque =
    case deque of
        EmptyDeque ->
            []

        Deque beginning middle end ->
            List.concat
                [ bufferToList beginning
                , toList middle
                , bufferToList end
                ]


bufferToList : Buffer a -> List a
bufferToList buffer =
    case buffer of
        EmptyBuffer ->
            []

        One a ->
            [ a ]

        Two a b ->
            [ a, b ]

        Three a b c ->
            [ a, b, c ]

        Four a b c d ->
            [ a, b, c, d ]
