module Internal.Buffer exposing
    ( Buffer(..)
    , foldl
    , foldr
    , length
    , map
    )


type Buffer a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
    | Five a a a a a
    | Six a a a a a a
    | Seven a a a a a a a


length : Buffer a -> Int
length buffer =
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

        Six _ _ _ _ _ _ ->
            6

        Seven _ _ _ _ _ _ _ ->
            7


foldl : (a -> b -> b) -> Buffer a -> b -> b
foldl fn buffer acc =
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

        Six a b c d e f ->
            fn f (fn e (fn d (fn c (fn b (fn a acc)))))

        Seven a b c d e f g ->
            fn g (fn f (fn e (fn d (fn c (fn b (fn a acc))))))


foldr : (a -> b -> b) -> Buffer a -> b -> b
foldr fn buffer acc =
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

        Six a b c d e f ->
            fn a (fn b (fn c (fn d (fn e (fn f acc)))))

        Seven a b c d e f g ->
            fn a (fn b (fn c (fn d (fn e (fn f (fn g acc))))))


map : (a -> b) -> Buffer a -> Buffer b
map fn buffer =
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

        Six a b c d e f ->
            Six (fn a) (fn b) (fn c) (fn d) (fn e) (fn f)

        Seven a b c d e f g ->
            Seven (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g)
