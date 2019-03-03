module Deque exposing
    ( Deque
    , empty
    , isEmpty
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
    Deque EmptyBuffer EmptyDeque EmptyBuffer


isEmpty : Deque a -> Bool
isEmpty deque =
    deque == empty
