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
    = Deque (Buffer a) (Deque a) (Buffer a)
    | Empty


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

        Deque (One e1) middle end ->
            Deque (Two element e1) middle end

        Deque (Two e1 e2) middle end ->
            Deque (Three element e1 e2) middle end

        Deque (Three e1 e2 e3) middle end ->
            Deque (Four element e1 e2 e3) middle end

        Deque (Four _ _ _ _) _ _ ->
            Deque (One element) deque BufferEmpty


pushBack : a -> Deque a -> Deque a
pushBack element deque =
    case deque of
        Empty ->
            Deque BufferEmpty Empty (One element)

        Deque start middle BufferEmpty ->
            Deque start middle (One element)

        Deque start middle (One e1) ->
            Deque start middle (Two e1 element)

        Deque start middle (Two e1 e2) ->
            Deque start middle (Three e1 e2 element)

        Deque start middle (Three e1 e2 e3) ->
            Deque start middle (Four e1 e2 e3 element)

        Deque _ _ (Four _ _ _ _) ->
            Deque BufferEmpty deque (One element)


popFront : Deque a -> ( Maybe a, Deque a )
popFront deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque BufferEmpty Empty BufferEmpty ->
            ( Nothing, Empty )

        Deque BufferEmpty middle BufferEmpty ->
            popFront middle

        Deque BufferEmpty Empty end ->
            popFront (Deque end Empty BufferEmpty)

        Deque BufferEmpty middle end ->
            let
                ( newBeginning, newMiddle ) =
                    popFrontDescend [] middle
            in
            popFront (Deque newBeginning newMiddle end)

        Deque (One e1) Empty BufferEmpty ->
            ( Just e1, Empty )

        Deque (One e1) middle end ->
            ( Just e1, Deque BufferEmpty middle end )

        Deque (Two e1 e2) middle end ->
            ( Just e1, Deque (One e2) middle end )

        Deque (Three e1 e2 e3) middle end ->
            ( Just e1, Deque (Two e2 e3) middle end )

        Deque (Four e1 e2 e3 e4) middle end ->
            ( Just e1, Deque (Three e2 e3 e4) middle end )


popFrontDescend : List (Deque a) -> Deque a -> ( Buffer a, Deque a )
popFrontDescend crumbs deque =
    case deque of
        Empty ->
            ( BufferEmpty, Empty )

        Deque BufferEmpty Empty BufferEmpty ->
            popAscend crumbs ( BufferEmpty, Empty )

        Deque BufferEmpty Empty end ->
            popAscend crumbs ( end, Empty )

        Deque BufferEmpty middle end ->
            popFrontDescend (deque :: crumbs) middle

        Deque beginning middle end ->
            popAscend crumbs ( beginning, Deque BufferEmpty middle end )


popAscend : List (Deque a) -> ( Buffer a, Deque a ) -> ( Buffer a, Deque a )
popAscend crumbs (( popped, newMiddle ) as result) =
    case crumbs of
        [] ->
            result

        first :: rest ->
            case first of
                Empty ->
                    ( BufferEmpty, Empty )

                Deque BufferEmpty _ BufferEmpty ->
                    popAscend rest ( popped, newMiddle )

                Deque beginning _ end ->
                    popAscend rest ( popped, Deque beginning newMiddle end )


popBack : Deque a -> ( Maybe a, Deque a )
popBack deque =
    case deque of
        Empty ->
            ( Nothing, Empty )

        Deque BufferEmpty Empty BufferEmpty ->
            ( Nothing, Empty )

        Deque BufferEmpty middle BufferEmpty ->
            popBack middle

        Deque beginning Empty BufferEmpty ->
            popBack (Deque BufferEmpty Empty beginning)

        Deque beginning middle BufferEmpty ->
            let
                ( newEnd, newMiddle ) =
                    popBackDescend [] middle
            in
            popBack (Deque beginning newMiddle newEnd)

        Deque BufferEmpty Empty (One e1) ->
            ( Just e1, Empty )

        Deque beginning middle (One e1) ->
            ( Just e1, Deque beginning middle BufferEmpty )

        Deque beginning middle (Two e1 e2) ->
            ( Just e2, Deque beginning middle (One e1) )

        Deque beginning middle (Three e1 e2 e3) ->
            ( Just e3, Deque beginning middle (Two e1 e2) )

        Deque beginning middle (Four e1 e2 e3 e4) ->
            ( Just e4, Deque beginning middle (Three e1 e2 e3) )


popBackDescend : List (Deque a) -> Deque a -> ( Buffer a, Deque a )
popBackDescend crumbs deque =
    case deque of
        Empty ->
            ( BufferEmpty, Empty )

        Deque beginning Empty BufferEmpty ->
            popAscend crumbs ( beginning, Empty )

        Deque beginning middle BufferEmpty ->
            popBackDescend (deque :: crumbs) middle

        Deque beginning middle end ->
            popAscend crumbs ( end, Deque beginning middle BufferEmpty )


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
            foldlDescend fn (bufferFoldl fn acc beginning) (end :: crumbs) middle


foldlAscend : (a -> b -> b) -> b -> List (Buffer a) -> b
foldlAscend fn acc crumbs =
    case crumbs of
        [] ->
            acc

        buffer :: next ->
            foldlAscend fn (bufferFoldl fn acc buffer) next


bufferFoldl : (a -> b -> b) -> b -> Buffer a -> b
bufferFoldl fn acc buffer =
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
    foldrDescend fn acc [] deque


foldrDescend : (a -> b -> b) -> b -> List (Buffer a) -> Deque a -> b
foldrDescend fn acc crumbs deque =
    case deque of
        Empty ->
            foldrAscend fn acc crumbs

        Deque beginning middle end ->
            foldrDescend fn (bufferFoldr fn acc end) (beginning :: crumbs) middle


foldrAscend : (a -> b -> b) -> b -> List (Buffer a) -> b
foldrAscend fn acc crumbs =
    case crumbs of
        [] ->
            acc

        current :: next ->
            foldrAscend fn (bufferFoldr fn acc current) next


bufferFoldr : (a -> b -> b) -> b -> Buffer a -> b
bufferFoldr fn acc buffer =
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
