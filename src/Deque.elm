module Deque exposing
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


popFront : Deque a -> Maybe ( a, Deque a )
popFront deque =
    case deque of
        Empty ->
            Nothing

        Deque (Buffer.One a) middle end ->
            Just ( a, Deque Buffer.Empty middle end )

        Deque Buffer.Empty Empty (Buffer.One a) ->
            Just ( a, Empty )

        Deque Buffer.Empty middle end ->
            case popFront middle of
                Just ( a, newMiddle ) ->
                    Just ( a, Deque Buffer.Empty newMiddle end )

                Nothing ->
                    case end of
                        Buffer.One a ->
                            Just ( a, Empty )

                        _ ->
                            Nothing

        _ ->
            Nothing


popBack : Deque a -> Maybe ( a, Deque a )
popBack deque =
    case deque of
        Empty ->
            Nothing

        Deque beginning middle (Buffer.One a) ->
            Just ( a, Deque beginning middle Buffer.Empty )

        Deque (Buffer.One a) Empty Buffer.Empty ->
            Just ( a, Empty )

        Deque beginning middle Buffer.Empty ->
            case popBack middle of
                Just ( a, newMiddle ) ->
                    Just ( a, Deque beginning newMiddle Buffer.Empty )

                Nothing ->
                    case beginning of
                        Buffer.One a ->
                            Just ( a, Empty )

                        _ ->
                            Nothing

        _ ->
            Nothing


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
