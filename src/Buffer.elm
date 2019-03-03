module Buffer exposing
    ( Buffer(..)
    , foldl
    , foldr
    )


type Buffer a
    = Empty
    | One a
    | Two a a
    | Three a a a
    | Four a a a a


foldl : (a -> b -> b) -> b -> Buffer a -> b
foldl fn acc buffer =
    case buffer of
        Empty ->
            acc

        One a ->
            fn a acc

        Two a b ->
            fn b (fn a acc)

        Three a b c ->
            fn c (fn b (fn a acc))

        Four a b c d ->
            fn d (fn c (fn b (fn a acc)))


foldr : (a -> b -> b) -> b -> Buffer a -> b
foldr fn acc buffer =
    case buffer of
        Empty ->
            acc

        One a ->
            fn a acc

        Two a b ->
            fn a (fn b acc)

        Three a b c ->
            fn a (fn b (fn c acc))

        Four a b c d ->
            fn a (fn b (fn c (fn d acc)))
