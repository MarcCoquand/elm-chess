module Predicate exposing
    ( Predicate
    , check
    , choose
    , contramap
    , divide
    , divideWithDefault
    , make
    , withDefault
    )

import Either exposing (Either)


type alias Predicate a =
    a -> Bool


check : Predicate a -> a -> Bool
check checker value =
    checker value


make : (a -> Bool) -> Predicate a
make predicate =
    predicate



{-
   These functions were an exploration in the use of Contravariance for moves.
   It seemed to have just made the code more complicated but might be worth
   looking into again.
-}


contramap : (a -> c) -> Predicate c -> Predicate a
contramap cf checker =
    \a ->
        checker (cf a)


choose : (a -> Either b c) -> Predicate b -> Predicate c -> Predicate a
choose cf bCondition cCondition =
    \a ->
        let
            eitherBC =
                cf a
        in
        case eitherBC of
            Either.Left b ->
                check bCondition b

            Either.Right c ->
                check cCondition c


divide : (a -> ( c, d )) -> Predicate c -> Predicate d -> Predicate a
divide cf cCondition dCondition =
    \a ->
        let
            ( c, d ) =
                cf a
        in
        check cCondition c && check dCondition d


{-| Special case of choose
-}
withDefault : Bool -> (a -> Maybe b) -> Predicate b -> Predicate a
withDefault bool cf bCondition =
    \a ->
        case cf a of
            Just b ->
                check bCondition b

            Nothing ->
                bool


divideWithDefault : Bool -> (a -> Maybe ( c, d )) -> Predicate c -> Predicate d -> Predicate a
divideWithDefault bool cf cCondition dCondition =
    withDefault bool cf (divide (\x -> x) cCondition dCondition)
