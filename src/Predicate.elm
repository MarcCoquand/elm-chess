module Predicate exposing
    ( Predicate
    , check
    , contramap
    , make
    )


type Predicate a
    = Predicate (a -> Bool)


check : Predicate a -> a -> Bool
check (Predicate checker) value =
    checker value


make : (a -> Bool) -> Predicate a
make predicate =
    Predicate predicate



-- There might be a neater abstraction to combine multiple predicates and sets
-- Using these


contramap : (a -> c) -> Predicate c -> Predicate a
contramap cf (Predicate checker) =
    Predicate
        (\a ->
            checker (cf a)
        )


combine : (a -> ( c, d )) -> Predicate c -> Predicate d -> Predicate a
combine cf (Predicate cChecker) (Predicate dChecker) =
    -- A.K.A. divide from Haskell
    Predicate
        (\a ->
            let
                ( c, d ) =
                    cf a
            in
            cChecker c && dChecker d
        )
