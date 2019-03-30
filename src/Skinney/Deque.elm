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

        Deque Buffer.Empty middle end ->
            Deque (Buffer.One element) middle end

        Deque (Buffer.One e1) middle end ->
            Deque (Buffer.Two element e1) middle end

        Deque (Buffer.Two e1 e2) middle end ->
            Deque (Buffer.Three element e1 e2) middle end

        Deque (Buffer.Three e1 e2 e3) middle end ->
            Deque (Buffer.Four element e1 e2 e3) middle end

        Deque (Buffer.Four _ _ _ _) _ _ ->
            Deque (Buffer.One element) deque Buffer.Empty


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Deque Buffer.Empty Empty (Buffer.One element)

        Deque start middle Buffer.Empty ->
            Deque start middle (Buffer.One element)

        Deque start middle (Buffer.One e1) ->
            Deque start middle (Buffer.Two e1 element)

        Deque start middle (Buffer.Two e1 e2) ->
            Deque start middle (Buffer.Three e1 e2 element)

        Deque start middle (Buffer.Three e1 e2 e3) ->
            Deque start middle (Buffer.Four e1 e2 e3 element)

        Deque _ _ (Buffer.Four _ _ _ _) ->
            Deque Buffer.Empty deque (Buffer.One element)


popFront : Deque a -> ( Maybe a, Deque a )
popFront deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque Buffer.Empty Empty Buffer.Empty ->
            ( Nothing, Empty )

        Deque Buffer.Empty middle Buffer.Empty ->
            popFront middle

        Deque Buffer.Empty Empty end ->
            popFront (Deque end Empty Buffer.Empty)

        Deque Buffer.Empty middle end ->
            let
                ( newBeginning, newMiddle ) =
                    popFrontDescend [] middle
            in
            popFront (Deque newBeginning newMiddle end)

        Deque (Buffer.One e1) Empty Buffer.Empty ->
            ( Just e1, Empty )

        Deque (Buffer.One e1) middle end ->
            ( Just e1, Deque Buffer.Empty middle end )

        Deque (Buffer.Two e1 e2) middle end ->
            ( Just e1, Deque (Buffer.One e2) middle end )

        Deque (Buffer.Three e1 e2 e3) middle end ->
            ( Just e1, Deque (Buffer.Two e2 e3) middle end )

        Deque (Buffer.Four e1 e2 e3 e4) middle end ->
            ( Just e1, Deque (Buffer.Three e2 e3 e4) middle end )


popFrontDescend : List (Deque a) -> Deque a -> ( Buffer a, Deque a )
popFrontDescend crumbs deque =
    case deque of
        Empty ->
            ( Buffer.Empty, Empty )

        Deque Buffer.Empty Empty Buffer.Empty ->
            popAscend crumbs ( Buffer.Empty, Empty )

        Deque Buffer.Empty Empty end ->
            popAscend crumbs ( end, Empty )

        Deque Buffer.Empty middle end ->
            popFrontDescend (deque :: crumbs) middle

        Deque beginning middle end ->
            popAscend crumbs ( beginning, Deque Buffer.Empty middle end )


popAscend : List (Deque a) -> ( Buffer a, Deque a ) -> ( Buffer a, Deque a )
popAscend crumbs (( popped, newMiddle ) as result) =
    case crumbs of
        [] ->
            result

        first :: rest ->
            case first of
                Empty ->
                    ( Buffer.Empty, Empty )

                Deque Buffer.Empty _ Buffer.Empty ->
                    popAscend rest ( popped, newMiddle )

                Deque beginning _ end ->
                    popAscend rest ( popped, Deque beginning newMiddle end )


popBack : Deque a -> ( Maybe a, Deque a )
popBack deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque Buffer.Empty Empty Buffer.Empty ->
            ( Nothing, Empty )

        Deque Buffer.Empty middle Buffer.Empty ->
            popBack middle

        Deque beginning Empty Buffer.Empty ->
            popBack (Deque Buffer.Empty Empty beginning)

        Deque beginning middle Buffer.Empty ->
            let
                ( newEnd, newMiddle ) =
                    popBackDescend [] middle
            in
            popBack (Deque beginning newMiddle newEnd)

        Deque Buffer.Empty Empty (Buffer.One e1) ->
            ( Just e1, Empty )

        Deque beginning middle (Buffer.One e1) ->
            ( Just e1, Deque beginning middle Buffer.Empty )

        Deque beginning middle (Buffer.Two e1 e2) ->
            ( Just e2, Deque beginning middle (Buffer.One e1) )

        Deque beginning middle (Buffer.Three e1 e2 e3) ->
            ( Just e3, Deque beginning middle (Buffer.Two e1 e2) )

        Deque beginning middle (Buffer.Four e1 e2 e3 e4) ->
            ( Just e4, Deque beginning middle (Buffer.Three e1 e2 e3) )


popBackDescend : List (Deque a) -> Deque a -> ( Buffer a, Deque a )
popBackDescend crumbs deque =
    case deque of
        Empty ->
            ( Buffer.Empty, Empty )

        Deque beginning Empty Buffer.Empty ->
            popAscend crumbs ( beginning, Empty )

        Deque beginning middle Buffer.Empty ->
            popBackDescend (deque :: crumbs) middle

        Deque beginning middle end ->
            popAscend crumbs ( end, Deque beginning middle Buffer.Empty )


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
