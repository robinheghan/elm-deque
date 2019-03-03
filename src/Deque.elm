module Deque exposing
    ( Deque
    , empty
    , foldl
    , foldr
    , fromList
    , isEmpty
    , pushBack
    , pushFront
    , singleton
    , toList
    )

import Buffer exposing (Buffer)


type Deque a
    = Deque (Buffer a) (Deque a) (Buffer a)
    | Empty


empty : Deque a
empty =
    Empty


isEmpty : Deque a -> Bool
isEmpty deque =
    deque == empty


singleton : a -> Deque a
singleton element =
    Deque (Buffer.One element) Empty Buffer.Empty


pushFront : a -> Deque a -> Deque a
pushFront element deque =
    case deque of
        Empty ->
            Deque (Buffer.One element) Empty Buffer.Empty

        nextMiddle ->
            Deque (Buffer.One element) nextMiddle Buffer.Empty


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Deque Buffer.Empty Empty (Buffer.One element)

        nextMiddle ->
            Deque Buffer.Empty nextMiddle (Buffer.One element)


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
            Buffer.foldl fn (foldl fn (Buffer.foldl fn acc beginning) middle) end


foldr : (a -> b -> b) -> b -> Deque a -> b
foldr fn acc deque =
    case deque of
        Empty ->
            acc

        Deque beginning middle end ->
            Buffer.foldr fn (foldr fn (Buffer.foldr fn acc end) middle) beginning
