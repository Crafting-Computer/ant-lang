module AntChecker exposing (..)

import AntParser
    exposing
        ( Accessor(..)
        , Block
        , CleanExpr(..)
        , Decl(..)
        , Expr(..)
        , FunctionDecl
        , Literal(..)
        , PathSegment(..)
        , Pattern(..)
        , Stmt(..)
        , Type(..)
        , Variable
        )
import Dict exposing (Dict)
import Location exposing (Located, changeLocation, dummyLocated, getLocationFromList, showLocation, withLocation)


type Problem
    = DuplicatedStructDecl (Located String) (Located String)
    | DuplicatedFunctionDecl (Located String) (Located String)
    | DuplicatedVariable (Located String) (Located String)
    | UndefinedNamedType (Located String)
    | UndefinedStructType (Located String)
    | UndefinedVariable (Located String)
    | UndefinedNamespace (Located String)
    | UndefinedFunction (Located String)
    | MutateImmutableVariable (Located String)
    | MismatchedTypes (Located Type) (Located Type)
    | ExpectingFunctionCall (Located Expr)
    | ExpectingBoolType (Located Type)
    | ExpectingIntType (Located Type)
    | ExpectingArrayType (Located Type)
    | ExpectingStructType (Located Type)
    | ExpectingfunctionType (Located Type)
    | ExpectingStructField (Located String) (Located Type)
    | MissingStructFields (Dict String ( Located String, Located Type )) (Located Type)
    | ExtraStructField (Located String)
    | ExtraPathSegments (List (Located PathSegment))
    | ExtraArguments (List (Located Expr))
    | MissingArguments (List ( Located Pattern, Located Type )) (List (Located Expr))


checkDecls : List Decl -> Result (List Problem) ()
checkDecls ds =
    let
        ( problems, declared ) =
            List.foldl
                (\d ( ps, { declaredTypes, declaredFunctions } as ds1 ) ->
                    case d of
                        StructDecl { name, fields } ->
                            let
                                next =
                                    ( ps
                                    , { ds1
                                        | declaredTypes =
                                            Dict.insert
                                                name.value
                                                (withLocation name <| StructType { name = name, fields = fields })
                                                ds1.declaredTypes
                                      }
                                    )
                            in
                            case Dict.get name.value declaredTypes of
                                Just prevTy ->
                                    case prevTy.value of
                                        StructType prevStructTy ->
                                            ( DuplicatedStructDecl prevStructTy.name name :: ps
                                            , ds1
                                            )

                                        _ ->
                                            next

                                Nothing ->
                                    next

                        FnDecl ({ name } as functionDecl) ->
                            case Dict.get name.value declaredFunctions of
                                Just prevFunction ->
                                    ( DuplicatedFunctionDecl prevFunction.name name :: ps
                                    , ds1
                                    )

                                Nothing ->
                                    ( ps
                                    , { ds1
                                        | declaredFunctions =
                                            Dict.insert name.value
                                                (substituteNamedTypeInFunctionDecl ds1 functionDecl)
                                                ds1.declaredFunctions
                                      }
                                    )

                        ImplDecl { target, functions } ->
                            List.foldl
                                (\functionDecl ( ps2, ds2 ) ->
                                    let
                                        next =
                                            ( ps2
                                            , { ds2
                                                | declaredFunctions =
                                                    Dict.insert functionDecl.name.value
                                                        (substituteNamedTypeInFunctionDecl ds2 functionDecl)
                                                        ds1.declaredFunctions
                                              }
                                            )
                                    in
                                    case Dict.get functionDecl.name.value declaredFunctions of
                                        Just prevFunction ->
                                            case prevFunction.namespace of
                                                Just namespace ->
                                                    if namespace.value == target.value then
                                                        ( DuplicatedFunctionDecl prevFunction.name functionDecl.name :: ps2
                                                        , ds2
                                                        )

                                                    else
                                                        next

                                                Nothing ->
                                                    next

                                        Nothing ->
                                            next
                                )
                                ( [], ds1 )
                                functions
                )
                ( [], { declaredTypes = Dict.empty, declaredFunctions = Dict.empty, declaredVariables = Dict.empty } )
                ds
    in
    case
        (List.concat <| List.map (checkDecl declared) ds)
            ++ problems
    of
        [] ->
            Ok ()

        ps ->
            Err ps


type alias Declarations =
    { declaredTypes : Dict String (Located Type)
    , declaredFunctions : Dict String FunctionDecl
    , declaredVariables : Dict String ( Variable, Located Type )
    }


checkDecl : Declarations -> Decl -> List Problem
checkDecl ds d =
    case d of
        StructDecl { name, fields } ->
            Dict.foldl
                (\_ ( fieldName, fieldType ) ps ->
                    case fieldType.value of
                        NamedType fieldTypeName ->
                            if not <| Dict.member fieldTypeName.value ds.declaredTypes then
                                UndefinedNamedType fieldTypeName :: ps

                            else
                                ps

                        _ ->
                            ps
                )
                []
                fields

        FnDecl f ->
            checkFunctionDecl ds f

        ImplDecl { functions } ->
            List.concat <| List.map (checkFunctionDecl ds) functions


checkFunctionDecl : Declarations -> FunctionDecl -> List Problem
checkFunctionDecl ds functionDecl =
    let
        { name, parameters, returnType, body } =
            functionDecl

        ( ps3, ds2 ) =
            List.foldl
                (\( paramPattern, paramType ) ( ps1, ds1 ) ->
                    case paramPattern.value of
                        IdentifierPattern param ->
                            case getVariable param.name.value ds1 of
                                Just ( prevParam, _ ) ->
                                    ( DuplicatedVariable prevParam.name param.name :: ps1
                                    , ds1
                                    )

                                Nothing ->
                                    case substituteNamedType ds1 ps1 paramType of
                                        Ok substType ->
                                            ( ps1
                                            , addVariable param substType ds1
                                            )

                                        Err ps2 ->
                                            ( ps2
                                            , ds1
                                            )

                        _ ->
                            case substituteNamedType ds1 ps1 paramType of
                                Ok _ ->
                                    ( ps1, ds1 )

                                Err ps2 ->
                                    ( ps2, ds1 )
                )
                ( [], ds )
                parameters

        -- add this function as variable to allow recursive calls in function body
        ds3 =
            addVariable { name = name, mutable = False }
                (withLocation name <| FunctionType <| substituteNamedTypeInFunctionDecl ds functionDecl)
                ds2
    in
    case getTypeFromBlock ds3 body of
        Ok ( bodyType, _ ) ->
            case substituteNamedType ds3 ps3 returnType of
                Ok substType ->
                    if areEqualTypes substType bodyType then
                        ps3

                    else
                        MismatchedTypes substType bodyType :: ps3

                Err ps4 ->
                    ps4

        Err ps2 ->
            ps2 ++ ps3


substituteNamedType :
    Declarations
    -> List Problem
    -> Located Type
    -> Result (List Problem) (Located Type)
substituteNamedType ds ps ty =
    case ty.value of
        NamedType typeName ->
            case getType typeName.value ds of
                Just substType ->
                    Ok <| changeLocation ty <| substType

                Nothing ->
                    Err <| UndefinedNamedType typeName :: ps

        _ ->
            Ok ty


substituteNamedTypeInFunctionDecl :
    Declarations
    -> FunctionDecl
    -> FunctionDecl
substituteNamedTypeInFunctionDecl ds { name, namespace, parameters, returnType, body } =
    let
        substParameters =
            List.map
                (\( paramName, paramType ) ->
                    case substituteNamedType ds [] paramType of
                        Ok substType ->
                            ( paramName, substType )

                        Err _ ->
                            ( paramName, paramType )
                )
                parameters

        substReturnType =
            case substituteNamedType ds [] returnType of
                Ok substType ->
                    substType
                
                Err _ ->
                    returnType
    in
    { name = name
    , namespace = namespace
    , parameters = substParameters
    , returnType = substReturnType
    , body = body
    }


checkBlock : Declarations -> Located Block -> List Problem
checkBlock ds b =
    case getTypeFromBlock ds b of
        Ok _ ->
            []

        Err ps ->
            ps


getTypeFromBlock : Declarations -> Located Block -> Result (List Problem) ( Located Type, Declarations )
getTypeFromBlock ds b =
    let
        ( stmts, e ) =
            b.value
    in
    List.foldl
        (\stmt ( ps, prevDeclarations ) ->
            let
                ( problems, declarations ) =
                    checkStmt prevDeclarations stmt
            in
            ( problems ++ ps, declarations )
        )
        ( [], ds )
        stmts
        |> (\( problems, ds1 ) ->
                case problems of
                    [] ->
                        case e of
                            Just expr ->
                                case getTypeFromExpr ds1 expr of
                                    Ok ok ->
                                        Ok ok

                                    Err ps ->
                                        Err ps

                            Nothing ->
                                Ok ( withLocation b UnitType, ds1 )

                    _ ->
                        Err problems
           )


checkStmt : Declarations -> Stmt -> ( List Problem, Declarations )
checkStmt ds s =
    case s of
        VarStmt { target, value } ->
            case target.value of
                IdentifierPattern targetVar ->
                    case getVariable targetVar.name.value ds of
                        Just ( prevVar, _ ) ->
                            ( [ DuplicatedVariable prevVar.name targetVar.name ]
                            , ds
                            )

                        Nothing ->
                            case getTypeFromExpr ds value of
                                Ok ( ty, ds1 ) ->
                                    ( []
                                    , addVariable targetVar ty ds1
                                    )

                                Err ps ->
                                    ( ps, ds )

                WildcardPattern ->
                    checkExpr ds value

        LetStmt { target, value } ->
            let
                targetName =
                    target.value.name
            in
            case getVariable targetName.value ds of
                Just ( declaredVar, declaredRootType ) ->
                    if not declaredVar.mutable then
                        ( [ MutateImmutableVariable targetName ]
                        , ds
                        )

                    else
                        let
                            declaredType =
                                List.foldl
                                    (\accessor result ->
                                        result
                                            |> Result.andThen
                                                (\ty ->
                                                    case accessor of
                                                        StructAccess fieldName ->
                                                            case ty.value of
                                                                StructType { fields } ->
                                                                    case Dict.get fieldName.value fields of
                                                                        Just ( _, fieldType ) ->
                                                                            Ok <| changeLocation fieldName fieldType

                                                                        Nothing ->
                                                                            Err
                                                                                [ ExpectingStructField fieldName <|
                                                                                    changeLocation fieldName ty
                                                                                ]

                                                                _ ->
                                                                    Err [ ExpectingStructType ty ]

                                                        ArrayAccess index ->
                                                            case ty.value of
                                                                ArrayType ->
                                                                    case getTypeFromExpr ds index of
                                                                        Ok ( indexType, _ ) ->
                                                                            case indexType.value of
                                                                                IntType ->
                                                                                    Ok <| changeLocation index indexType

                                                                                -- all array elements are integers
                                                                                _ ->
                                                                                    Err [ ExpectingIntType indexType ]

                                                                        Err ps ->
                                                                            Err ps

                                                                _ ->
                                                                    Err [ ExpectingArrayType ty ]
                                                )
                                    )
                                    (Ok <| changeLocation targetName declaredRootType)
                                    target.value.accessors
                        in
                        case declaredType of
                            Ok targetType ->
                                case getTypeFromExpr ds value of
                                    Ok ( ty, ds1 ) ->
                                        if not <| areEqualTypes targetType ty then
                                            ( [ MismatchedTypes targetType ty ]
                                            , ds1
                                            )

                                        else
                                            ( [], ds1 )

                                    Err ps ->
                                        ( ps, ds )

                            Err ps ->
                                ( ps, ds )

                Nothing ->
                    ( [ UndefinedVariable targetName ]
                    , ds
                    )

        CallStmt e ->
            case e.value of
                CallExpr _ ->
                    checkExpr ds e

                _ ->
                    ( [ ExpectingFunctionCall e ]
                    , ds
                    )

        ReturnStmt e ->
            checkExpr ds e


checkExpr : Declarations -> Located Expr -> ( List Problem, Declarations )
checkExpr ds e =
    case getTypeFromExpr ds e of
        Ok ( _, ds1 ) ->
            ( [], ds1 )

        Err ps ->
            ( ps, ds )


getTypeFromExpr : Declarations -> Located Expr -> Result (List Problem) ( Located Type, Declarations )
getTypeFromExpr ds e =
    Result.map (Tuple.mapFirst (withLocation e)) <|
        case e.value of
            IfExpr { condition, thenBody, elseBody } ->
                getTypeFromExpr ds condition
                    |> Result.andThen
                        (\( conditionType, ds1 ) ->
                            case conditionType.value of
                                BoolType ->
                                    getTypeFromBlock ds1 thenBody
                                        |> Result.andThen
                                            (\( thenType, ds2 ) ->
                                                getTypeFromExpr ds2 elseBody
                                                    |> Result.andThen
                                                        (\( elseType, ds3 ) ->
                                                            if areEqualTypes thenType elseType then
                                                                Ok ( elseType.value, ds3 )

                                                            else
                                                                Err [ MismatchedTypes thenType elseType ]
                                                        )
                                            )

                                _ ->
                                    Err [ ExpectingBoolType conditionType ]
                        )

            WhileExpr { condition, body } ->
                getTypeFromExpr ds condition
                    |> Result.andThen
                        (\( conditionType, ds1 ) ->
                            case conditionType.value of
                                BoolType ->
                                    getTypeFromBlock ds1 body
                                        |> Result.map
                                            (\( bodyType, ds2 ) ->
                                                ( bodyType.value, ds2 )
                                            )

                                _ ->
                                    Err [ ExpectingBoolType conditionType ]
                        )

            LiteralExpr literal ->
                let
                    primitive ty =
                        Ok ( ty, ds )
                in
                case literal.value of
                    CharLiteral _ ->
                        primitive CharType

                    StringLiteral _ ->
                        primitive StringType

                    IntLiteral _ ->
                        primitive IntType

                    BoolLiteral _ ->
                        primitive BoolType

                    StructLiteral { name, fields } ->
                        case getType name.value ds of
                            Just ty ->
                                case ty.value of
                                    StructType structType ->
                                        let
                                            unimplementedFields =
                                                Dict.diff structType.fields fields
                                        in
                                        if Dict.isEmpty unimplementedFields then
                                            Result.map
                                                (\( fields2, ds3 ) ->
                                                    ( StructType
                                                        { name = name
                                                        , fields = fields2
                                                        }
                                                    , ds3
                                                    )
                                                )
                                            <|
                                                Dict.foldl
                                                    (\_ ( fieldName, fieldValue ) result ->
                                                        result
                                                            |> Result.andThen
                                                                (\( fieldTypes, ds1 ) ->
                                                                    case Dict.get fieldName.value structType.fields of
                                                                        Just ( _, fieldType ) ->
                                                                            case getTypeFromExpr ds1 fieldValue of
                                                                                Ok ( valueType, ds2 ) ->
                                                                                    if areEqualTypes fieldType valueType then
                                                                                        Ok
                                                                                            ( Dict.insert
                                                                                                fieldName.value
                                                                                                ( fieldName, valueType )
                                                                                                fieldTypes
                                                                                            , ds2
                                                                                            )

                                                                                    else
                                                                                        Err [ MismatchedTypes fieldType valueType ]

                                                                                Err ps ->
                                                                                    Err ps

                                                                        Nothing ->
                                                                            Err [ ExtraStructField fieldName ]
                                                                )
                                                    )
                                                    (Ok ( Dict.empty, ds ))
                                                    fields

                                        else
                                            Err [ MissingStructFields unimplementedFields ty ]

                                    _ ->
                                        Err [ UndefinedStructType name ]

                            Nothing ->
                                Err [ UndefinedStructType name ]

            ArithmeticExpr { left, right } ->
                getTypeFromBinaryInt ds left right IntType

            ComparisonExpr { left, right } ->
                getTypeFromBinaryInt ds left right BoolType

            BooleanExpr { left, right } ->
                getTypeFromBinaryBool ds left right

            CallExpr { caller, arguments } ->
                getTypeFromExpr ds caller
                    |> Result.andThen
                        (\( callerType, ds1 ) ->
                            case callerType.value of
                                FunctionType f ->
                                    let
                                        unspecifiedParameters =
                                            List.drop (List.length arguments) f.parameters
                                    in
                                    if List.isEmpty unspecifiedParameters then
                                        let
                                            extraArguments =
                                                List.drop (List.length f.parameters) arguments
                                        in
                                        if List.isEmpty extraArguments then
                                            (\( problems, ds4 ) ->
                                                case problems of
                                                    [] ->
                                                        Ok ( f.returnType.value, ds4 )

                                                    _ ->
                                                        Err problems
                                            )
                                            <|
                                                List.foldl
                                                    (\( ( _, paramType ), argument ) ( ps1, ds2 ) ->
                                                        case getTypeFromExpr ds2 argument of
                                                            Ok ( argType, ds3 ) ->
                                                                if areEqualTypes paramType argType then
                                                                    ( ps1, ds3 )

                                                                else
                                                                    ( MismatchedTypes paramType argType :: ps1
                                                                    , ds3
                                                                    )

                                                            Err ps2 ->
                                                                ( ps2 ++ ps1, ds2 )
                                                    )
                                                    ( [], ds1 )
                                                    (List.map2 Tuple.pair f.parameters arguments)

                                        else
                                            Err [ ExtraArguments extraArguments ]

                                    else
                                        Err [ MissingArguments unspecifiedParameters arguments ]

                                _ ->
                                    Err [ ExpectingfunctionType callerType ]
                        )

            PlaceExpr { target, accessor } ->
                case accessor of
                    StructAccess fieldName ->
                        getTypeFromExpr ds target
                            |> Result.andThen
                                (\( ty, ds1 ) ->
                                    case ty.value of
                                        StructType { fields } ->
                                            case Dict.get fieldName.value fields of
                                                Just ( _, fieldType ) ->
                                                    Ok ( fieldType.value, ds1 )

                                                Nothing ->
                                                    Err [ ExpectingStructField fieldName ty ]

                                        _ ->
                                            Err [ ExpectingStructType ty ]
                                )

                    ArrayAccess index ->
                        getTypeFromExpr ds target
                            |> Result.andThen
                                (\( ty, ds1 ) ->
                                    case ty.value of
                                        ArrayType ->
                                            getTypeFromExpr ds1 index
                                                |> Result.andThen
                                                    (\( indexType, ds2 ) ->
                                                        case indexType.value of
                                                            IntType ->
                                                                Ok ( ArrayType, ds2 )

                                                            _ ->
                                                                Err [ ExpectingIntType indexType ]
                                                    )

                                        _ ->
                                            Err [ ExpectingArrayType ty ]
                                )

            PathExpr ( firstSegment, restSegments ) ->
                case restSegments of
                    [] ->
                        case firstSegment.value of
                            IdentifierSegment name ->
                                case getVariable name.value ds of
                                    Just ( _, ty ) ->
                                        Ok ( ty.value, ds )

                                    Nothing ->
                                        Err [ UndefinedVariable name ]

                    secondSegment :: thirdOnwardSegments ->
                        case thirdOnwardSegments of
                            [] ->
                                case firstSegment.value of
                                    IdentifierSegment name ->
                                        case getNamespace name.value ds of
                                            Just functions ->
                                                case secondSegment.value of
                                                    IdentifierSegment functionName ->
                                                        case Dict.get functionName.value functions of
                                                            Just function ->
                                                                Ok ( FunctionType function, ds )

                                                            Nothing ->
                                                                Err [ UndefinedFunction functionName ]

                                            Nothing ->
                                                Err [ UndefinedNamespace name ]

                            _ ->
                                Err [ ExtraPathSegments thirdOnwardSegments ]

            BlockExpr block ->
                Result.map (Tuple.mapFirst .value) <|
                    getTypeFromBlock ds <|
                        dummyLocated block


getTypeFromBinaryInt : Declarations -> Located Expr -> Located Expr -> Type -> Result (List Problem) ( Type, Declarations )
getTypeFromBinaryInt ds left right t =
    getTypeFromExpr ds left
        |> Result.andThen
            (\( leftType, ds1 ) ->
                case leftType.value of
                    IntType ->
                        getTypeFromExpr ds1 right
                            |> Result.andThen
                                (\( rightType, ds2 ) ->
                                    case rightType.value of
                                        IntType ->
                                            Ok ( t, ds2 )

                                        _ ->
                                            Err [ ExpectingIntType leftType ]
                                )

                    _ ->
                        Err [ ExpectingIntType leftType ]
            )


getTypeFromBinaryBool : Declarations -> Located Expr -> Located Expr -> Result (List Problem) ( Type, Declarations )
getTypeFromBinaryBool ds left right =
    getTypeFromExpr ds left
        |> Result.andThen
            (\( leftType, ds1 ) ->
                case leftType.value of
                    BoolType ->
                        getTypeFromExpr ds1 right
                            |> Result.andThen
                                (\( rightType, ds2 ) ->
                                    case rightType.value of
                                        BoolType ->
                                            Ok ( BoolType, ds2 )

                                        _ ->
                                            Err [ ExpectingBoolType leftType ]
                                )

                    _ ->
                        Err [ ExpectingBoolType leftType ]
            )


areEqualTypes : Located Type -> Located Type -> Bool
areEqualTypes t1 t2 =
    case ( t1.value, t2.value ) of
        ( BoolType, BoolType ) ->
            True

        ( IntType, IntType ) ->
            True

        ( StringType, StringType ) ->
            True

        ( CharType, CharType ) ->
            True

        ( UnitType, UnitType ) ->
            True

        ( StructType s1, StructType s2 ) ->
            s1.name.value == s2.name.value

        _ ->
            False


getVariable : String -> Declarations -> Maybe ( Variable, Located Type )
getVariable name ds =
    Dict.get name ds.declaredVariables


addVariable : Variable -> Located Type -> Declarations -> Declarations
addVariable variable ty ds =
    { ds
        | declaredVariables =
            Dict.insert variable.name.value ( variable, ty ) ds.declaredVariables
    }


getType : String -> Declarations -> Maybe (Located Type)
getType name ds =
    Dict.get name ds.declaredTypes


getNamespace : String -> Declarations -> Maybe (Dict String FunctionDecl)
getNamespace name ds =
    (\dict ->
        if Dict.isEmpty dict then
            Nothing

        else
            Just dict
    )
    <|
        Dict.filter
            (\_ { namespace } ->
                case namespace of
                    Just name2 ->
                        name2.value == name

                    Nothing ->
                        False
            )
            ds.declaredFunctions


showProblems : String -> List Problem -> String
showProblems src problems =
    String.join "\n\n" <| List.map (showProblem src) problems


showProblem : String -> Problem -> String
showProblem src p =
    String.join "\n" <|
        case p of
            DuplicatedStructDecl n1 n2 ->
                showDuplicatedProblem "struct" n1 n2 src

            DuplicatedFunctionDecl n1 n2 ->
                showDuplicatedProblem "function" n1 n2 src

            DuplicatedVariable n1 n2 ->
                showDuplicatedProblem "variable" n1 n2 src

            UndefinedNamedType n ->
                showUndefinedProblem "type" n src

            UndefinedStructType n ->
                showUndefinedProblem "struct" n src

            UndefinedVariable n ->
                showUndefinedProblem "variable" n src

            UndefinedNamespace n ->
                showUndefinedProblem "namespace" n src

            UndefinedFunction n ->
                showUndefinedProblem "function" n src

            MutateImmutableVariable n ->
                [ "-- MUTATE IMMUTABLE VARIABLE"
                , "I found that you are trying to mutate an immutable variable " ++ n.value ++ " here:"
                , showLocation src n
                , showHintIntro
                , "1. Change the implementation to avoid mutation."
                , "2. Make the variable " ++ n.value ++ " mutable using the 'mut' keyword."
                ]

            MismatchedTypes expectedType actualType ->
                [ "-- MISMATCHED TYPES"
                , "I'm expecting the type " ++ showType expectedType ++ " here:"
                , showLocation src actualType
                , "but found the type " ++ showType actualType ++ " instead"
                ]

            ExpectingFunctionCall e ->
                [ "-- EXPECTING FUNCTION CALL"
                , "I'm expecting a function call here:"
                , showLocation src e
                , showHintIntro
                , "1. Change the expression into a function call."
                ]

            ExpectingBoolType n ->
                showExpectingTypeProblem "Bool" n src

            ExpectingIntType n ->
                showExpectingTypeProblem "Int" n src

            ExpectingArrayType n ->
                showExpectingTypeProblem "Array" n src

            ExpectingStructType n ->
                showExpectingTypeProblem "struct" n src

            ExpectingfunctionType n ->
                showExpectingTypeProblem "function" n src

            ExpectingStructField fieldName actualType ->
                [ "-- EXPECTING STRUCT FIELD"
                , "I'm expecting a field " ++ fieldName.value ++ " here:"
                , showLocation src actualType
                , "but found a " ++ showType actualType
                , showHintIntro
                , "1. Did you make a typo? Try picking a field name that exists in the struct."
                , "2. Maybe your struct is the problem? Try adding the missing field to your struct."
                ]

            MissingStructFields fields actualType ->
                [ "-- MISSING STRUCT FIELDS"
                , "I found that you missed " ++ (String.fromInt <| Dict.size fields) ++ " required struct fields here:"
                , showLocation src <| actualType
                , "Here are the missing fields:"
                , Dict.foldl
                    (\_ ( fieldName, fieldType ) str ->
                        str ++ fieldName.value ++ " : " ++ showType fieldType ++ "\n"
                    )
                    ""
                    fields
                , showHintIntro
                , "1. Implement the missing fields."
                , "2. Maybe you mean to use another struct? Try changing the struct type."
                ]

            ExtraStructField fieldName ->
                [ "-- EXTRA STRUCT FIELDS"
                , "I found an extra struct field " ++ fieldName.value ++ " here:"
                , showLocation src fieldName
                , showHintIntro
                , "1. Remove the extra field."
                ]

            ExtraPathSegments segments ->
                [ "-- EXTRA PATH SEGMENTS"
                , "I found " ++ (String.fromInt <| List.length segments) ++ " extra path segments here:"
                , showLocation src <| getLocationFromList segments
                , showHintIntro
                , "1. If you are referring to a variable, you should have only 1 segment for the variable name"
                , "2. If you are referring to a namespace function, you should have only 2 segments. The first for the namespace name, the second for the function name."
                ]

            ExtraArguments arguments ->
                [ "-- EXTRA ARGUMENTS"
                , "I found " ++ (String.fromInt <| List.length arguments) ++ " extra function arguments here:"
                , showLocation src <| getLocationFromList arguments
                , showHintIntro
                , "1. Remove the extra arguments."
                , "2. Reduce the function parameters to fit your arguments."
                ]

            MissingArguments parameters arguments ->
                [ "-- MISSING ARGUMENTS"
                , "I'm expecting " ++ (String.fromInt <| List.length parameters) ++ " more arguments here:"
                , showLocation src <| getLocationFromList arguments
                , "The missing arguments are:"
                , List.foldl
                    (\( paramName, paramType ) str ->
                        str ++ "\n" ++ showPattern paramName ++ " : " ++ showType paramType
                    )
                    ""
                    parameters
                , showHintIntro
                , "1. Add the missing arguments."
                , "2. Increase the function parameters to fit your arguments."
                ]


showPattern : Located Pattern -> String
showPattern p =
    case p.value of
        IdentifierPattern { name, mutable } ->
            if mutable then
                "mut "

            else
                "" ++ name.value

        WildcardPattern ->
            "_"


showType : Located Type -> String
showType t =
    case t.value of
        IntType ->
            "Int"

        BoolType ->
            "Bool"

        CharType ->
            "Char"

        StringType ->
            "String"

        UnitType ->
            "()"

        NamedType n ->
            n.value

        StructType { name, fields } ->
            name.value
                ++ " {"
                ++ indentStr
                    (Dict.foldl
                        (\_ ( fieldName, fieldType ) str ->
                            str ++ "\n" ++ fieldName.value ++ " : " ++ showType fieldType ++ ","
                        )
                        ""
                        fields
                    )
                ++ "\n}"

        FunctionType { name, parameters, returnType } ->
            "fn "
                ++ name.value
                ++ " ("
                ++ indentStr
                    (List.foldl
                        (\( _, paramType ) str ->
                            str ++ "\n" ++ showType paramType ++ ","
                        )
                        ""
                        parameters
                    )
                ++ "\n)"
                ++ "\n: "
                ++ showType returnType

        ArrayType ->
            "Array"


indentStr : String -> String
indentStr str =
    String.replace "\n" "\n    " str


showDuplicatedProblem : String -> Located String -> Located String -> String -> List String
showDuplicatedProblem kind n1 n2 src =
    [ "-- DUPLICATED " ++ String.toUpper kind
    , "I found that you declared " ++ kind ++ " " ++ n1.value ++ " twice."
    , "First time here:"
    , showLocation src n1
    , "Second time here:"
    , showLocation src n2
    , showHintIntro
    , "1. Remove one of the declarations."
    , "2. Rename one of the declarations."
    ]


showUndefinedProblem : String -> Located String -> String -> List String
showUndefinedProblem kind n src =
    [ "-- UNDEFINED " ++ String.toUpper kind
    , "I found an undefined " ++ kind ++ " " ++ n.value ++ " here:"
    , showLocation src n
    , showHintIntro
    , "1. Change " ++ n.value ++ " to refer to a defined " ++ kind ++ "."
    , "2. Define " ++ n.value ++ "."
    ]


showExpectingTypeProblem : String -> Located Type -> String -> List String
showExpectingTypeProblem kind t src =
    [ "-- EXPECTING " ++ String.toUpper kind
    , "I'm expecting a " ++ kind ++ " here:"
    , showLocation src t
    , "but found the type " ++ showType t
    , showHintIntro
    , "1. Change the type into a " ++ kind ++ "."
    ]


showHintIntro : String
showHintIntro =
    "Hint: You can try one of the following fixes:"
