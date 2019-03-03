module Buffer exposing
    ( Buffer(..)
    , toList
    )


type Buffer a
    = Empty
    | One a
    | Two a a
    | Three a a a
    | Four a a a a


toList : Buffer a -> List a
toList buffer =
    case buffer of
        Empty ->
            []

        One a ->
            [ a ]

        Two a b ->
            [ a, b ]

        Three a b c ->
            [ a, b, c ]

        Four a b c d ->
            [ a, b, c, d ]
