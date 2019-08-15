module Predicate exposing
    ( BelongsToPlayer(..)
    , IsBlank(..)
    , IsCollision(..)
    , OutOfBounds(..)
    , Predicate
    , Swappable(..)
    , Threatened(..)
    , check
    , contramap
    , make
    )


type Predicate a b
    = Predicate (a -> Bool)


type IsCollision
    = IsCollision


type BelongsToPlayer
    = BelongsToPlayer


type Threatened
    = Threatened


type IsBlank
    = IsBlank


type Swappable
    = Swappable


type OutOfBounds
    = OutOfBounds


check : Predicate a b -> a -> Bool
check (Predicate checker) value =
    checker value


make : (a -> Bool) -> b -> Predicate a b
make predicate _ =
    Predicate predicate



-- There might be a neater abstraction to combine multiple predicates and sets
-- Using these


contramap : (a -> c) -> Predicate c b -> Predicate a b
contramap cf (Predicate checker) =
    Predicate
        (\a ->
            checker (cf a)
        )


combine : (a -> ( c, d )) -> Predicate c m -> Predicate d n -> Predicate a o
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
