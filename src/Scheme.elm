module Scheme exposing
    ( Exp(..), EnvOp, evalWithEnv
    , quote, begin, define, setBang, isNull, car, cdr
    , DictEnv, dictEnvOp, evalWithDictEnv
    , ProcEnv, procEnvOp, evalWithProcEnv
    )

{-|


# Interpreter

@docs Exp, EnvOp, evalWithEnv


# Primitives

@docs quote, begin, define, setBang, isNull, car, cdr


# Dict environment

@docs DictEnv, dictEnvOp, evalWithDictEnv


# Procedure environment

@docs ProcEnv, procEnvOp, evalWithProcEnv

-}

import Dict exposing (Dict)



-- INTERPRETER


type Exp
    = T -- True
    | F -- False
    | Num Float -- Number
    | Str String -- String
    | Err String -- Error
    | Sym String -- Symbol
    | Lst (List Exp) -- List


type alias EnvOp env =
    { envContains : String -> env -> Bool
    , lookupEnv : String -> env -> Exp
    , extendEnv : String -> Exp -> env -> env
    }


evalWithEnv : Exp -> env -> EnvOp env -> ( Exp, env )
evalWithEnv exp env envOp =
    case exp of
        T ->
            ( exp, env )

        F ->
            ( exp, env )

        Num _ ->
            ( exp, env )

        Str _ ->
            ( exp, env )

        Err _ ->
            ( exp, env )

        Sym symbol ->
            evalSymbol symbol env envOp
                |> withEnv env

        Lst list ->
            case list of
                (Sym "quote") :: xs ->
                    evalQuote xs
                        |> withEnv env

                (Sym "begin") :: xs ->
                    evalBegin xs env envOp

                (Sym "define") :: xs ->
                    evalDefine xs env envOp

                (Sym "setBang") :: xs ->
                    evalSetBang xs env envOp

                (Sym "isNull") :: xs ->
                    evalIsNull xs env envOp
                        |> withEnv env

                (Sym "cons") :: xs ->
                    evalCons xs env envOp
                        |> withEnv env

                (Sym "car") :: xs ->
                    evalCar xs env envOp
                        |> withEnv env

                (Sym "cdr") :: xs ->
                    evalCdr xs env envOp
                        |> withEnv env

                _ ->
                    Debug.todo ""


evalSymbol : String -> env -> EnvOp env -> Exp
evalSymbol symbol env { lookupEnv } =
    lookupEnv symbol env


evalQuote : List Exp -> Exp
evalQuote args =
    case args of
        [ exp ] ->
            exp

        _ ->
            Err ("`quote` takes one argument but " ++ argsSize args ++ " was given.")


evalBegin : List Exp -> env -> EnvOp env -> ( Exp, env )
evalBegin args env envOp =
    List.foldl (\exp ( _, newEnv ) -> evalWithEnv exp newEnv envOp) ( Err "", env ) args


evalDefine : List Exp -> env -> EnvOp env -> ( Exp, env )
evalDefine args env ({ envContains, extendEnv } as envOp) =
    case args of
        [ Sym symbol, exp ] ->
            if envContains symbol env then
                Err ("`" ++ symbol ++ "` is already defined.")
                    |> withEnv env

            else
                Sym symbol
                    |> withEnv (extendEnv symbol (evalToExp exp env envOp) env)

        [ exp, _ ] ->
            Err ("The first argument to `define` must be an symbol but " ++ expType exp ++ " was given.")
                |> withEnv env

        _ ->
            Err ("`define` takes two arguments but " ++ argsSize args ++ " was given.")
                |> withEnv env


evalSetBang : List Exp -> env -> EnvOp env -> ( Exp, env )
evalSetBang args env ({ envContains, extendEnv } as envOp) =
    case args of
        [ Sym symbol, exp ] ->
            if envContains symbol env then
                Sym symbol
                    |> withEnv (extendEnv symbol (evalToExp exp env envOp) env)

            else
                unboundVar symbol
                    |> withEnv env

        [ exp, _ ] ->
            Err ("The first argument to `setBang` must be an symbol but " ++ expType exp ++ " was given.")
                |> withEnv env

        _ ->
            Err ("`setBang` takes two arguments but " ++ argsSize args ++ " was given.")
                |> withEnv env


evalIsNull : List Exp -> env -> EnvOp env -> Exp
evalIsNull args env envOp =
    case args of
        [ exp ] ->
            case evalToExp exp env envOp of
                Lst [] ->
                    T

                _ ->
                    F

        _ ->
            Err ("`isNull` takes one argument but " ++ argsSize args ++ " was given.")


evalCons : List Exp -> env -> EnvOp env -> Exp
evalCons args env envOp =
    case args of
        [ exp, listExp ] ->
            case evalToExp listExp env envOp of
                Lst list ->
                    Lst (evalToExp exp env envOp :: list)

                evaluatedListExp ->
                    Err ("The second argument to `cons` must be a list but " ++ expType evaluatedListExp ++ " was given.")

        _ ->
            Err ("`cons` takes two arguments but " ++ argsSize args ++ " was given.")


evalCar : List Exp -> env -> EnvOp env -> Exp
evalCar args env envOp =
    case args of
        [ exp ] ->
            case evalToExp exp env envOp of
                Lst (x :: _) ->
                    x

                Lst [] ->
                    Err "`car` is defined only for non-empty lists but empty list was given."

                evaluatedListExp ->
                    Err ("`car` is defined only for non-empty lists but " ++ expType evaluatedListExp ++ " was given.")

        _ ->
            Err ("`car` takes one argument but " ++ argsSize args ++ " was given.")


evalCdr : List Exp -> env -> EnvOp env -> Exp
evalCdr args env envOp =
    case args of
        [ exp ] ->
            case evalToExp exp env envOp of
                Lst (_ :: xs) ->
                    Lst xs

                Lst [] ->
                    Err "`cdr` is defined only for non-empty lists but empty list was given."

                evaluatedListExp ->
                    Err ("`cdr` is defined only for non-empty lists but " ++ expType evaluatedListExp ++ " was given.")

        _ ->
            Err ("`cdr` takes one argument but " ++ argsSize args ++ " was given.")



-- PRIMITIVES


quote : Exp -> Exp
quote exp =
    Lst [ Sym "quote", exp ]


begin : List Exp -> Exp
begin exps =
    Lst (Sym "begin" :: exps)


define : String -> Exp -> Exp
define symbol exp =
    Lst [ Sym "define", Sym symbol, exp ]


setBang : String -> Exp -> Exp
setBang symbol exp =
    Lst [ Sym "setBang", Sym symbol, exp ]


isNull : Exp -> Exp
isNull exp =
    Lst [ Sym "isNull", exp ]


car : Exp -> Exp
car exp =
    Lst [ Sym "car", exp ]


cdr : Exp -> Exp
cdr exp =
    Lst [ Sym "cdr", exp ]



-- DICT ENVIRONMENT


type alias DictEnv =
    Dict String Exp


dictEnvOp : EnvOp DictEnv
dictEnvOp =
    { envContains = Dict.member
    , lookupEnv =
        \symbol env ->
            Dict.get symbol env
                |> Maybe.withDefault (unboundVar symbol)
    , extendEnv = Dict.insert
    }


evalWithDictEnv : Exp -> Exp
evalWithDictEnv exp =
    evalToExp exp Dict.empty dictEnvOp



-- PROCEDURE ENVIRONMENT


type alias ProcEnv =
    String -> Exp


procEnvOp : EnvOp ProcEnv
procEnvOp =
    { envContains = Debug.todo ""
    , lookupEnv = Debug.todo ""
    , extendEnv = Debug.todo ""
    }


evalWithProcEnv : Exp -> Exp
evalWithProcEnv exp =
    Debug.todo ""



-- HELPERS


evalToExp : Exp -> env -> EnvOp env -> Exp
evalToExp exp env envOp =
    Tuple.first (evalWithEnv exp env envOp)


unboundVar : String -> Exp
unboundVar symbol =
    Err ("Unbound variable: `" ++ symbol ++ "`.")


withEnv : env -> Exp -> ( Exp, env )
withEnv env exp =
    ( exp, env )


argsSize : List a -> String
argsSize =
    String.fromInt << List.length


expType : Exp -> String
expType exp =
    case exp of
        T ->
            "boolean"

        F ->
            "boolean"

        Err _ ->
            "error"

        Num _ ->
            "number"

        Str _ ->
            "string"

        Sym _ ->
            "symbol"

        Lst _ ->
            "list"
