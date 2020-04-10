module Interpreter exposing (Env, Exp, Value(..), apply, call, eval, lambda, var)

import Dict exposing (Dict)


type alias Env =
    Dict String Value


type Exp
    = V String -- Variable
    | L String Exp -- Lambda
    | A Exp Exp -- Application


var : String -> Exp
var =
    V


lambda : List String -> Exp -> Exp
lambda args body =
    List.foldr L body args


apply : List Exp -> Exp -> Exp
apply fns arg =
    List.foldr A arg fns


type Value
    = Int Int
    | Chr Char
    | Str String
    | Bln Bool
    | Cls String Exp Env -- Closure
    | Err String


eval : Exp -> Env -> Value
eval exp env =
    case exp of
        V name ->
            Dict.get name env
                |> Maybe.withDefault (Err "Variable not found")

        L arg body ->
            Cls arg body env

        A rator rand ->
            call (eval rator env) (eval rand env)


call : Value -> Value -> Value
call fun x =
    case fun of
        Cls arg body env ->
            eval body (Dict.insert arg x env)

        _ ->
            Err "I can call only functions"
