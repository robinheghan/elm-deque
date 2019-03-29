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

import Skinney.Buffer as Buffer exposing (Buffer)


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


popFront : Deque a -> ( Maybe a, Deque a )
popFront deque =
    popFrontDescend [] deque


popFrontDescend : List (Deque a) -> Deque a -> ( Maybe a, Deque a )
popFrontDescend crumbs deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque (Buffer.One a) middle end ->
            popAscend crumbs ( Just a, Deque Buffer.Empty middle end )

        Deque Buffer.Empty Empty (Buffer.One a) ->
            popAscend crumbs ( Just a, Empty )

        Deque Buffer.Empty middle end ->
            popFrontDescend (deque :: crumbs) middle

        _ ->
            ( Nothing, Empty )


popAscend : List (Deque a) -> ( Maybe a, Deque a ) -> ( Maybe a, Deque a )
popAscend crumbs (( popped, newMiddle ) as result) =
    case crumbs of
        [] ->
            result

        first :: rest ->
            case first of
                Empty ->
                    ( Nothing, Empty )

                Deque Buffer.Empty _ Buffer.Empty ->
                    popAscend rest ( popped, newMiddle )

                Deque beginning _ end ->
                    popAscend rest ( popped, Deque beginning newMiddle end )


popBack : Deque a -> ( Maybe a, Deque a )
popBack deque =
    popBackDescend [] deque


popBackDescend : List (Deque a) -> Deque a -> ( Maybe a, Deque a )
popBackDescend crumbs deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque Buffer.Empty Empty (Buffer.One a) ->
            popAscend crumbs ( Just a, Empty )

        Deque (Buffer.One a) Empty Buffer.Empty ->
            popAscend crumbs ( Just a, Empty )

        Deque beginning middle (Buffer.One a) ->
            popAscend crumbs ( Just a, Deque beginning middle Buffer.Empty )

        Deque beginning middle Buffer.Empty ->
            popBackDescend (deque :: crumbs) middle

        _ ->
            ( Nothing, Empty )


fromList : List a -> Deque a
fromList list =
    List.foldl pushBack empty list


toList : Deque a -> List a
toList deque =
    foldr (::) [] deque


foldl : (a -> b -> b) -> b -> Deque a -> b
foldl fn acc deque =
    foldlDescend fn acc [] deque


foldlDescend : (a -> b -> b) -> b -> List (Buffer a) -> Deque a -> b
foldlDescend fn acc crumbs deque =
    case deque of
        Empty ->
            foldlAscend fn acc crumbs

        Deque beginning middle end ->
            foldlDescend fn (Buffer.foldl fn acc beginning) (end :: crumbs) middle


foldlAscend : (a -> b -> b) -> b -> List (Buffer a) -> b
foldlAscend fn acc crumbs =
    case crumbs of
        [] ->
            acc

        buffer :: next ->
            foldlAscend fn (Buffer.foldl fn acc buffer) next


foldr : (a -> b -> b) -> b -> Deque a -> b
foldr fn acc deque =
    foldrDescend fn acc [] deque


foldrDescend : (a -> b -> b) -> b -> List (Buffer a) -> Deque a -> b
foldrDescend fn acc crumbs deque =
    case deque of
        Empty ->
            foldrAscend fn acc crumbs

        Deque beginning middle end ->
            foldrDescend fn (Buffer.foldr fn acc end) (beginning :: crumbs) middle


foldrAscend : (a -> b -> b) -> b -> List (Buffer a) -> b
foldrAscend fn acc crumbs =
    case crumbs of
        [] ->
            acc

        current :: next ->
            foldrAscend fn (Buffer.foldr fn acc current) next


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
