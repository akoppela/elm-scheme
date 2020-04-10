module Core exposing
    ( id, flip, compose, compose2
    , true, false, not, and, or, eqBool
    , zero, isZero, one, add1, add, sub1, sub, mult, pow, leqNum, eqNum, gtNum
    , pair, first, second
    )

{-|


# Basics

@docs id, flip, compose, compose2


# Bool

@docs true, false, not, and, or, eqBool


# Peano numbers

@docs zero, isZero, one, add1, add, sub1, sub, mult, pow, leqNum, eqNum, gtNum


# Pair

@docs pair, first, second

-}

import Dict exposing (Dict)
import Interpreter exposing (..)
import Variable exposing (..)


{-| Combinator: Idiot
-}
id : Exp
id =
    lambda [ a ] vA


{-| Combinator: Cardinal
-}
flip : Exp
flip =
    lambda [ f ] <|
        apply [ vF, false ] true


{-| Combinator: Bluebird
-}
compose : Exp
compose =
    lambda [ f, g, a ] <|
        apply [ vF ] <|
            apply [ vG ] vA


{-| Combinator: Blackbird
-}
compose2 : Exp
compose2 =
    apply [ compose, compose ] compose



-- {-| Combinator: Y
-- -}
-- rec : Exp
-- rec =
--     A (L "f" (L "x" (A (V "f") (A (V "x") (V "x"))))) (L "x" (A (V "f") (A (V "x") (V "x"))))
-- recA : Exp
-- recA =
--     L "f" (A or (L "x" (A (V "f") (A or (V "x")))))
--- BOOL


{-| Combinator: Kestrel
-}
true : Exp
true =
    lambda [ a, b ] vA


{-| Combinator: Kite
-}
false : Exp
false =
    apply [ true ] id


{-| Same as flip
-}
not : Exp
not =
    flip


and : Exp
and =
    lambda [ a, b ] <|
        apply [ vA, vB ] vA


{-| Combinator: Mockingbird
-}
or : Exp
or =
    lambda [ f ] <|
        apply [ vF ] vF


eqBool : Exp
eqBool =
    lambda [ a, b ] <|
        apply [ vA, vB ] <|
            apply [ not ] vB



-- NUMBER


zero : Exp
zero =
    false


isZero : Exp
isZero =
    lambda [ n ] <|
        apply
            [ apply [ vN, true ] false
            ]
            true


one : Exp
one =
    id


add1 : Exp
add1 =
    lambda [ n, f ] <|
        apply [ compose, vF ] <|
            apply [ vN ] vF


add : Exp
add =
    lambda [ n, k ] <|
        apply [ vN, add1 ] vK


sub1 : Exp
sub1 =
    lambda [ n ] <|
        apply [ first ] <|
            apply [ vN, phi ] <|
                apply [ pair, zero ] zero


sub : Exp
sub =
    lambda [ n, k ] <|
        apply [ vK, sub1 ] vN


mult : Exp
mult =
    compose


{-| Combinator: Thrush
-}
pow : Exp
pow =
    apply [ flip ] id


phi : Exp
phi =
    lambda [ p ] <|
        apply
            [ pair
            , apply [ second ] vP
            ]
        <|
            apply [ add1 ] <|
                apply [ second ] vP


leqNum : Exp
leqNum =
    apply [ compose2, isZero ] sub


eqNum : Exp
eqNum =
    lambda [ n, k ] <|
        apply
            [ and
            , apply [ leqNum, vN ] vK
            ]
        <|
            apply [ leqNum, vK ] vN


gtNum : Exp
gtNum =
    apply [ compose2, not ] leqNum



-- PAIR


pair : Exp
pair =
    apply [ compose, not ] pow


first : Exp
first =
    lambda [ p ] <|
        apply [ vP ] true


second : Exp
second =
    lambda [ p ] <|
        apply [ vP ] false
