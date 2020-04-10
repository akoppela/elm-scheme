module Main exposing (main)

import Dict exposing (Dict)


main : Program () () ()
main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( (), Cmd msg )
init _ =
    let
        _ =
            Debug.log "" <|
                Scheme.eval (A fib Scheme.id) envTable
    in
    ( (), Cmd.none )


envTable : Dict String Value
envTable =
    Dict.fromList
        [ ( "x", Str "x" )
        , ( "y", Str "y" )
        ]


fib : Exp
fib =
    L "n" (a4 (V "n") (l3 "f" "a" "b" (a2 (V "f") (V "b") (a2 add (V "a") (V "b")))) true zero one)


toElmInt : ((Int -> Int) -> Int -> Int) -> Int
toElmInt c =
    c (\x -> x + 1) 0
