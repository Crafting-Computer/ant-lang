module AntChecker exposing (..)

import AntParser
    exposing
        ( Accessor(..)
        , Block
        , CleanExpr(..)
        , CleanNamespace
        , Decl(..)
        , Expr(..)
        , FunctionDecl
        , FunctionHeader
        , Generics
        , Literal(..)
        , Namespace(..)
        , PathSegment(..)
        , Pattern(..)
        , Stmt(..)
        , TraitDeclaration
        , TraitFunctionDecl
        , Type(..)
        , Variable
        , cleanNamespace
        )
import AssocList as AllDict
import Dict exposing (Dict)
import Location exposing (Located, changeLocation, dummyLocated, getLocationFromList, showLocation, withLocation)


type Problem
    = DuplicatedStructDecl (Located String) (Located String)
    | DuplicatedFunctionDecl (Located String) (Located String)
    | DuplicatedVariable (Located String) (Located String)
    | DuplicatedTraitDecl (Located String) (Located String)
    | UndefinedNamedType (Located String)
    | UndefinedStructType (Located String)
    | UndefinedVariable (Located String)
    | UndefinedNamespace Namespace
    | UndefinedFunction (Located String)
    | UndefinedTrait (Located String)
    | MutateImmutableVariable (Located String)
    | MismatchedTypes (Located Type) (Located Type)
    | ExpectingFunctionCall (Located Expr)
    | ExpectingBoolType (Located Type)
    | ExpectingIntType (Located Type)
    | ExpectingArrayType (Located Type)
    | ExpectingStructType (Located Type)
    | ExpectingfunctionType (Located Type)
    | ExpectingStructField (Located String) (Located Type)
    | MissingTraitFunction TraitFunctionDecl (Located String)
    | MissingStructFields (Dict String ( Located String, Located Type )) (Located Type)
    | ExtraStructField (Located String)
    | ExtraPathSegments (List (Located PathSegment))
    | ExtraArguments (List (Located Expr))
    | MissingArguments (List ( Located Pattern, Located Type )) (Located (List (Located Expr)))
    | MissingGenerics (List ( Located String, Dict String (Located String) )) (Located (List (Located Type)))
    | MissingTraits (List (Located String)) (Located Type)
    | ExtraGenerics (List (Located Type))
    | UnexpectedTraitInPathSegment (Located String)
    | UnexpectedGenericTypesInPathSegment (Located String) (Located (List (Located Type)))


checkDecls : List Decl -> Result (List Problem) ()
checkDecls decls =
    let
        ( problems, declared ) =
            List.foldl
                (\d ( ps, { declaredTypes } as ds1 ) ->
                    case d of
                        StructDecl ({ name } as struct) ->
                            let
                                next =
                                    ( ps
                                    , { ds1
                                        | declaredTypes =
                                            Dict.insert
                                                name.value
                                                (withLocation name <| StructType struct)
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

                        FnDecl ({ namespace, generics, name } as functionDecl) ->
                            let
                                ds2 =
                                    addGenericTypes generics ds1
                            in
                            case getFunction name.value namespace ds2 of
                                Just prevFunction ->
                                    ( DuplicatedFunctionDecl prevFunction.name name :: ps
                                    , ds2
                                    )

                                Nothing ->
                                    ( ps
                                    , addFunction (substituteNamedTypeInFunctionHeader ds2 functionDecl) ds2
                                    )

                        ImplDecl { target, generics, trait, functions } ->
                            (\( ps3, ds3 ) ->
                                case trait of
                                    Nothing ->
                                        ( ps3, ds3 )

                                    Just t ->
                                        ( ps3
                                        , { ds3
                                            | implementedTraits =
                                                case Dict.get target.value ds3.implementedTraits of
                                                    Just traits ->
                                                        Dict.insert
                                                            target.value
                                                            (t.value :: traits)
                                                            ds3.implementedTraits

                                                    Nothing ->
                                                        Dict.insert
                                                            target.value
                                                            [ t.value ]
                                                            ds3.implementedTraits
                                          }
                                        )
                            )
                            <|
                                let
                                    genericTypes =
                                        genericsToGenericTypes generics

                                    ds2 =
                                        addGenericTypes generics ds1
                                in
                                Dict.foldl
                                    (\_ functionDecl ( ps2, ds4 ) ->
                                        let
                                            next =
                                                ( ps2
                                                , addFunction
                                                    (substituteNamedTypeInFunctionHeader ds4 <|
                                                        substituteSelfTypeInFunctionHeader
                                                            (withLocation target <|
                                                                NamedType target <|
                                                                    genericTypes
                                                            )
                                                        <|
                                                            functionDecl
                                                    )
                                                    ds4
                                                )
                                        in
                                        case getFunction functionDecl.name.value functionDecl.namespace ds4 of
                                            Just prevFunction ->
                                                if areEqualNamespaces functionDecl.namespace prevFunction.namespace then
                                                    ( DuplicatedFunctionDecl prevFunction.name functionDecl.name :: ps2
                                                    , ds4
                                                    )

                                                else
                                                    next

                                            Nothing ->
                                                next
                                    )
                                    ( [], ds2 )
                                    functions

                        TraitDecl { name, functions } ->
                            case getTrait name.value ds1 of
                                Just prevTrait ->
                                    ( DuplicatedTraitDecl prevTrait.name name :: ps
                                    , ds1
                                    )

                                Nothing ->
                                    Tuple.mapSecond
                                        (\fs ->
                                            { ds1
                                                | declaredTraits =
                                                    Dict.insert name.value
                                                        { name = name, functions = fs }
                                                        ds1.declaredTraits
                                            }
                                        )
                                    <|
                                        Dict.foldl
                                            (\_ functionDecl ( ps2, fs ) ->
                                                let
                                                    next =
                                                        ( ps2
                                                        , Dict.insert functionDecl.name.value
                                                            (substituteNamedTypeInFunctionHeader ds1 functionDecl)
                                                            fs
                                                        )
                                                in
                                                case Dict.get functionDecl.name.value fs of
                                                    Just prevFunction ->
                                                        ( DuplicatedFunctionDecl prevFunction.name functionDecl.name :: ps2
                                                        , fs
                                                        )

                                                    Nothing ->
                                                        next
                                            )
                                            ( [], Dict.empty )
                                            functions
                )
                ( []
                , { declaredTypes = Dict.empty
                  , declaredFunctions = AllDict.empty
                  , declaredVariables = Dict.empty
                  , declaredTraits = Dict.empty
                  , implementedTraits = Dict.empty
                  }
                )
                decls
    in
    case
        (List.concat <| List.map (checkDecl declared) decls)
            ++ problems
    of
        [] ->
            Ok ()

        ps ->
            Err ps


genericsToGenericTypes : Generics -> Located (List (Located Type))
genericsToGenericTypes generics =
    withLocation generics <|
        List.map
            (\( name, traits ) ->
                withLocation name <| GenericType name traits
            )
            generics.value


type alias Declarations =
    { declaredTypes : Dict String (Located Type)
    , declaredFunctions : AllDict.Dict CleanNamespace (Dict String FunctionDecl)
    , declaredTraits : Dict String TraitDeclaration
    , declaredVariables : Dict String ( Variable, Located Type )
    , implementedTraits : Dict String (List String)
    }


checkDecl : Declarations -> Decl -> List Problem
checkDecl ds d =
    case d of
        StructDecl { name, fields } ->
            Dict.foldl
                (\_ ( fieldName, fieldType ) ps ->
                    case fieldType.value of
                        NamedType fieldTypeName _ ->
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

        ImplDecl { target, trait, functions } ->
            let
                implFunctions =
                    Maybe.withDefault functions <|
                        -- impossible
                        getNamespace
                            (StructNamespace target trait)
                            ds
            in
            case getType target.value ds of
                Just targetType ->
                    (case trait of
                        Nothing ->
                            []

                        Just traitName ->
                            case getTrait traitName.value ds of
                                Nothing ->
                                    [ UndefinedTrait traitName ]

                                Just traitDeclaration ->
                                    Dict.foldl
                                        (\_ traitFunction ps ->
                                            case Dict.get traitFunction.name.value implFunctions of
                                                Nothing ->
                                                    MissingTraitFunction traitFunction target :: ps

                                                Just functionDecl ->
                                                    let
                                                        traitFunctionType =
                                                            withLocation traitDeclaration.name <|
                                                                FunctionType <|
                                                                    substituteSelfTypeInFunctionHeader targetType <|
                                                                        traitFunction

                                                        functionType =
                                                            withLocation functionDecl.name <|
                                                                FunctionType <|
                                                                    getHeaderFromFunctionDecl functionDecl
                                                    in
                                                    if areEqualTypes traitFunctionType functionType then
                                                        ps

                                                    else
                                                        MismatchedTypes traitFunctionType functionType :: ps
                                        )
                                        []
                                        traitDeclaration.functions
                    )
                        ++ (List.concat <|
                                List.map (checkFunctionDecl ds) <|
                                    Dict.values implFunctions
                           )

                Nothing ->
                    [ UndefinedNamedType target ]

        TraitDecl { functions } ->
            List.concat <|
                List.map (checkFunctionHeader ds) <|
                    Dict.values functions


checkFunctionHeader : Declarations -> FunctionHeader a -> List Problem
checkFunctionHeader ds f =
    let
        { parameters, returnType } =
            f

        ps3 =
            List.foldl
                (\( paramPattern, paramType ) ps1 ->
                    case paramPattern.value of
                        IdentifierPattern param ->
                            case getVariable param.name.value ds of
                                Just ( prevParam, _ ) ->
                                    DuplicatedVariable prevParam.name param.name :: ps1

                                Nothing ->
                                    case substituteNamedType ds ps1 paramType of
                                        Ok _ ->
                                            ps1

                                        Err ps2 ->
                                            ps2

                        _ ->
                            case substituteNamedType ds ps1 paramType of
                                Ok _ ->
                                    ps1

                                Err ps2 ->
                                    ps2
                )
                []
                parameters
    in
    case substituteNamedType ds ps3 returnType of
        Ok substType ->
            ps3

        Err ps4 ->
            ps4


checkFunctionDecl : Declarations -> FunctionDecl -> List Problem
checkFunctionDecl ds functionDecl =
    let
        { name, generics, parameters, returnType, body } =
            functionDecl

        ds0 =
            addGenericTypes generics ds

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
                ( [], ds0 )
                parameters

        -- add this function as variable to allow recursive calls in function body
        ds3 =
            addVariable { name = name, mutable = False }
                (withLocation name <|
                    FunctionType <|
                        getHeaderFromFunctionDecl <|
                            substituteNamedTypeInFunctionHeader ds functionDecl
                )
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
        NamedType typeName genericTypes ->
            case getType typeName.value ds of
                Just substType ->
                    case substType.value of
                        StructType structType ->
                            Result.map (withLocation ty) <|
                                substituteGenericsInStructType ds genericTypes structType

                        _ ->
                            if List.length genericTypes.value > 0 then
                                Err <| ExtraGenerics genericTypes.value :: ps

                            else
                                Ok <| changeLocation ty <| substType

                Nothing ->
                    Err <| UndefinedNamedType typeName :: ps

        StructType structType ->
            Result.map
                (\fields ->
                    withLocation ty <|
                        StructType
                            { structType
                                | fields =
                                    fields
                            }
                )
            <|
                Dict.foldl
                    (\_ ( fieldName, fieldType ) result ->
                        case result of
                            Err _ ->
                                result

                            Ok fields ->
                                substituteNamedType ds ps fieldType
                                    |> Result.map
                                        (\newFieldType ->
                                            Dict.insert
                                                fieldName.value
                                                ( fieldName
                                                , newFieldType
                                                )
                                                fields
                                        )
                    )
                    (Ok Dict.empty)
                    structType.fields

        _ ->
            Ok ty


substituteGenericsInStructType :
    Declarations
    -> Located (List (Located Type))
    ->
        { name : Located String
        , generics : Generics
        , fields : Dict String ( Located String, Located Type )
        }
    -> Result (List Problem) Type
substituteGenericsInStructType ds substTypes structType =
    let
        { generics, fields } =
            structType

        ps =
            checkSubstTypesForGenerics ds generics substTypes
    in
    case ps of
        [] ->
            let
                ds2 =
                    List.foldl
                        (\( generic, substType ) ds1 ->
                            let
                                genericName =
                                    Tuple.first generic
                            in
                            { ds1
                                | declaredTypes =
                                    Dict.insert
                                        genericName.value
                                        substType
                                        ds1.declaredTypes
                            }
                        )
                        ds
                        (List.map2 Tuple.pair generics.value substTypes.value)
            in
            Ok <|
                StructType
                    { structType
                        | fields =
                            Dict.map
                                (\_ ( fieldName, fieldType ) ->
                                    ( fieldName
                                    , Result.withDefault fieldType <|
                                        substituteNamedType ds2 [] fieldType
                                    )
                                )
                                fields
                    }

        _ ->
            Err ps


checkSubstTypesForGenerics : Declarations -> Generics -> Located (List (Located Type)) -> List Problem
checkSubstTypesForGenerics ds generics substTypes =
    let
        unspecifiedGenerics =
            List.drop (List.length substTypes.value) generics.value
    in
    if List.isEmpty unspecifiedGenerics then
        let
            extraGenericTypes =
                List.drop (List.length generics.value) substTypes.value
        in
        if List.isEmpty extraGenericTypes then
            List.foldl
                (\( generic, substType1 ) ps1 ->
                    case substituteNamedType ds ps1 substType1 of
                        Err ps2 ->
                            ps2 ++ ps1

                        Ok substType2 ->
                            let
                                implementedTraits =
                                    getImplementedTraits ds substType2

                                expectedTraits =
                                    Dict.values <| Tuple.second generic
                            in
                            List.foldl
                                (\expectedTrait missingTraits ->
                                    if List.member expectedTrait.value implementedTraits then
                                        missingTraits

                                    else
                                        expectedTrait :: missingTraits
                                )
                                []
                                expectedTraits
                                |> (\missingTraits ->
                                        MissingTraits missingTraits substType2 :: ps1
                                   )
                )
                []
                (List.map2 Tuple.pair generics.value substTypes.value)

        else
            [ ExtraGenerics extraGenericTypes ]

    else
        [ MissingGenerics unspecifiedGenerics substTypes ]


getImplementedTraits : Declarations -> Located Type -> List String
getImplementedTraits ds ty =
    case ty.value of
        StructType { name } ->
            case Dict.get name.value ds.implementedTraits of
                Just traits ->
                    traits

                Nothing ->
                    []

        _ ->
            []


substituteSelfTypeInFunctionHeader :
    Located Type
    -> FunctionHeader a
    -> FunctionHeader a
substituteSelfTypeInFunctionHeader substType header =
    let
        { parameters, returnType } =
            header

        substParameters =
            List.map
                (\( paramName, paramType ) ->
                    case paramType.value of
                        SelfType ->
                            ( paramName, substType )

                        _ ->
                            ( paramName, paramType )
                )
                parameters

        substReturnType =
            case returnType.value of
                SelfType ->
                    substType

                _ ->
                    returnType
    in
    { header
        | parameters = substParameters
        , returnType = substReturnType
    }


substituteNamedTypeInFunctionHeader :
    Declarations
    -> FunctionHeader a
    -> FunctionHeader a
substituteNamedTypeInFunctionHeader ds header =
    let
        { name, namespace, parameters, returnType } =
            header

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
    { header
        | name = name
        , namespace = namespace
        , parameters = substParameters
        , returnType = substReturnType
    }


getHeaderFromFunctionDecl : FunctionDecl -> FunctionHeader {}
getHeaderFromFunctionDecl { name, namespace, generics, parameters, returnType } =
    { name = name
    , namespace = namespace
    , generics = generics
    , parameters = parameters
    , returnType = returnType
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

                    StructLiteral { name, generics, fields } ->
                        case getType name.value ds of
                            Just ty ->
                                case ty.value of
                                    StructType structType ->
                                        let
                                            unimplementedFields =
                                                Dict.diff structType.fields fields
                                        in
                                        if Dict.isEmpty unimplementedFields then
                                            Result.andThen
                                                (\( fields2, ds3 ) ->
                                                    substituteGenericsInStructType
                                                        ds3
                                                        generics
                                                        { name = name
                                                        , generics = structType.generics
                                                        , fields = fields2
                                                        }
                                                        |> Result.map
                                                            (\substType ->
                                                                ( substType, ds3 )
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
                                            List.drop (List.length arguments.value) f.parameters
                                    in
                                    if List.isEmpty unspecifiedParameters then
                                        let
                                            extraArguments =
                                                List.drop (List.length f.parameters) arguments.value
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
                                                    (List.map2 Tuple.pair f.parameters arguments.value)

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
                                        case getFunction name.value ModuleNamespace ds of
                                            Just f ->
                                                Ok ( FunctionType <| getHeaderFromFunctionDecl f, ds )

                                            Nothing ->
                                                Err [ UndefinedVariable name ]

                            QualifiedSegment { trait } ->
                                Err [ UnexpectedTraitInPathSegment trait ]

                            GenericsSegment name genericTypes ->
                                Err [ UnexpectedGenericTypesInPathSegment name genericTypes ]

                    secondSegment :: thirdOnwardSegments ->
                        case thirdOnwardSegments of
                            [] ->
                                let
                                    namespace =
                                        case firstSegment.value of
                                            IdentifierSegment name ->
                                                StructNamespace name Nothing

                                            QualifiedSegment { struct, trait } ->
                                                StructNamespace struct (Just trait)

                                            GenericsSegment name genericTypes ->
                                                GenericStructNamespace name genericTypes
                                in
                                case getNamespace namespace ds of
                                    Just functions ->
                                        case secondSegment.value of
                                            IdentifierSegment functionName ->
                                                case Dict.get functionName.value functions of
                                                    Just function ->
                                                        Ok
                                                            ( FunctionType <| getHeaderFromFunctionDecl <| function
                                                            , ds
                                                            )

                                                    Nothing ->
                                                        Err [ UndefinedFunction functionName ]

                                            QualifiedSegment { trait } ->
                                                Err [ UnexpectedTraitInPathSegment trait ]

                                            GenericsSegment functionName substTypes ->
                                                case Dict.get functionName.value functions of
                                                    Just function ->
                                                        let
                                                            functionHeader =
                                                                getHeaderFromFunctionDecl <| function
                                                        in
                                                        case substituteGenericTypesInFunctionHeader ds substTypes functionHeader of
                                                            Ok substHeader ->
                                                                Ok
                                                                    ( FunctionType substHeader
                                                                    , ds
                                                                    )

                                                            Err ps ->
                                                                Err ps

                                                    Nothing ->
                                                        Err [ UndefinedFunction functionName ]

                                    Nothing ->
                                        Err [ UndefinedNamespace namespace ]

                            _ ->
                                Err [ ExtraPathSegments thirdOnwardSegments ]

            BlockExpr block ->
                Result.map (Tuple.mapFirst .value) <|
                    getTypeFromBlock ds <|
                        dummyLocated block


substituteGenericTypesInFunctionHeader :
    Declarations
    -> Located (List (Located Type))
    -> FunctionHeader {}
    -> Result (List Problem) (FunctionHeader {})
substituteGenericTypesInFunctionHeader ds substTypes header =
    let
        substitute ty =
            List.foldl
                (\( ( genericName, _ ), substType ) ty1 ->
                    substituteGenericTypes genericName.value substType ty1
                )
                ty
                (List.map2 Tuple.pair header.generics.value substTypes.value)
    in
    case checkSubstTypesForGenerics ds header.generics substTypes of
        [] ->
            Ok <|
                { header
                    | parameters =
                        List.map
                            (\( paramName, paramType ) ->
                                ( paramName, substitute paramType )
                            )
                            header.parameters
                    , returnType =
                        substitute header.returnType
                }

        ps ->
            Err ps


substituteGenericTypes : String -> Located Type -> Located Type -> Located Type
substituteGenericTypes genericName substType ty =
    case ty.value of
        GenericType name _ ->
            if name.value == genericName then
                changeLocation ty substType

            else
                ty

        StructType ({ fields } as structType) ->
            withLocation ty <|
                StructType
                    { structType
                        | fields =
                            Dict.map
                                (\_ ( fieldName, fieldType ) ->
                                    ( fieldName
                                    , substituteGenericTypes genericName substType fieldType
                                    )
                                )
                                fields
                    }

        _ ->
            ty


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

        ( ArrayType, ArrayType ) ->
            True

        ( FunctionType f1, FunctionType f2 ) ->
            List.length f1.parameters
                == List.length f2.parameters
                && List.all
                    (\( ( _, paramType1 ), ( _, paramType2 ) ) ->
                        areEqualTypes paramType1 paramType2
                    )
                    (List.map2 Tuple.pair f1.parameters f2.parameters)
                && areEqualTypes f1.returnType f2.returnType

        ( GenericType n1 _, GenericType n2 _ ) ->
            n1.value == n2.value

        _ ->
            False


areEqualNamespaces : Namespace -> Namespace -> Bool
areEqualNamespaces n1 n2 =
    case ( n1, n2 ) of
        ( ModuleNamespace, ModuleNamespace ) ->
            True

        ( StructNamespace s1 t1, StructNamespace s2 t2 ) ->
            s1.value
                == s2.value
                && (Maybe.map .value t1 == Maybe.map .value t2)

        ( TraitNamespace t1, TraitNamespace t2 ) ->
            t1.value == t2.value

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


addGenericTypes : Generics -> Declarations -> Declarations
addGenericTypes generics ds =
    let
        newDeclaredTypes =
            List.foldl
                (\( name, traits ) declaredTypes ->
                    Dict.insert
                        name.value
                        (withLocation name <| GenericType name traits)
                        declaredTypes
                )
                ds.declaredTypes
                generics.value
    in
    { ds
        | declaredTypes =
            newDeclaredTypes
    }


getType : String -> Declarations -> Maybe (Located Type)
getType name ds =
    Dict.get name ds.declaredTypes


getNamespace : Namespace -> Declarations -> Maybe (Dict String FunctionDecl)
getNamespace namespace ds =
    AllDict.get (cleanNamespace namespace) ds.declaredFunctions


getFunction : String -> Namespace -> Declarations -> Maybe FunctionDecl
getFunction name namespace ds =
    getNamespace namespace ds
        |> Maybe.andThen
            (\functions ->
                Dict.get name functions
            )


addFunction : FunctionDecl -> Declarations -> Declarations
addFunction f ds =
    case getNamespace f.namespace ds of
        Just functions ->
            { ds
                | declaredFunctions =
                    AllDict.insert (cleanNamespace f.namespace)
                        (Dict.insert f.name.value f functions)
                        ds.declaredFunctions
            }

        Nothing ->
            { ds
                | declaredFunctions =
                    AllDict.insert (cleanNamespace f.namespace)
                        (Dict.singleton f.name.value f)
                        ds.declaredFunctions
            }


getTrait : String -> Declarations -> Maybe TraitDeclaration
getTrait n ds =
    Dict.get n ds.declaredTraits


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

            DuplicatedTraitDecl n1 n2 ->
                showDuplicatedProblem "trait" n1 n2 src

            UndefinedNamedType n ->
                showUndefinedProblem "type" n src

            UndefinedStructType n ->
                showUndefinedProblem "struct" n src

            UndefinedVariable n ->
                showUndefinedProblem "variable" n src

            UndefinedNamespace namespace ->
                let
                    n =
                        case namespace of
                            ModuleNamespace ->
                                -- impossible
                                dummyLocated ""

                            StructNamespace struct trait ->
                                case trait of
                                    Just t ->
                                        { from =
                                            struct.from
                                        , to =
                                            t.to
                                        , value =
                                            "<" ++ struct.value ++ " as " ++ t.value ++ ">"
                                        }

                                    Nothing ->
                                        struct

                            TraitNamespace trait ->
                                trait

                            GenericStructNamespace name types ->
                                withLocation name <|
                                    name.value
                                        ++ "<"
                                        ++ (String.join ", " <| List.map showType types.value)
                                        ++ ">"
                in
                showUndefinedProblem "namespace" n src

            UndefinedFunction n ->
                showUndefinedProblem "function" n src

            UndefinedTrait n ->
                showUndefinedProblem "trait" n src

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

            MissingTraitFunction traitFunction implTarget ->
                [ "-- MISSING TRAIT FUNCTION"
                , "I found that you missed a trait function here:"
                , showLocation src implTarget
                , "I'm expecting a function with this header:"
                , showFunctionHeader traitFunction
                , showHintIntro
                , "1. Define the missing function " ++ traitFunction.name.value ++ "."
                , "2. Maybe you mean to implement another trait?"
                ]

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
                let
                    extraArgLength =
                        String.fromInt <| List.length arguments
                in
                [ "-- EXTRA ARGUMENTS"
                , "I found " ++ extraArgLength ++ " extra function arguments here:"
                , showLocation src <| getLocationFromList arguments
                , showHintIntro
                , "1. Remove the " ++ extraArgLength ++ " extra arguments."
                , "2. Increase the function parameters by " ++ extraArgLength ++ " to fit your arguments."
                ]

            MissingArguments parameters arguments ->
                let
                    extraParamLength =
                        String.fromInt <| List.length parameters
                in
                [ "-- MISSING ARGUMENTS"
                , "I'm expecting " ++ extraParamLength ++ " more arguments here:"
                , showLocation src arguments
                , "The missing arguments are:\n"
                , List.foldl
                    (\( paramName, paramType ) str ->
                        str ++ showPattern paramName ++ " : " ++ showType paramType ++ "\n"
                    )
                    ""
                    parameters
                , showHintIntro
                , "1. Add the missing " ++ extraParamLength ++ " arguments."
                , "2. Reduce the function parameters by " ++ extraParamLength ++ " to fit your arguments."
                ]

            UnexpectedTraitInPathSegment trait ->
                [ "-- UNEXPECTED TRAIT IN PATH SEGMENT"
                , "I'm not expecting a trait bound " ++ trait.value ++ " in path segment:"
                , showLocation src trait
                , showHintIntro
                , "1. Remove the trait bound"
                , "2. Maybe you mean to access a trait method like this?"
                , "   <Person as Display>::show"
                , "   In the first path segment, the trait bound 'Display' is used to specify which trait of the struct 'Person' we care about."
                , "   In the second path segment, we specify the method 'show' in the trait 'Display'."
                ]

            MissingGenerics generics types ->
                let
                    extraGenericsLength =
                        String.fromInt <| List.length generics
                in
                [ "-- MISSING GENERICS"
                , "I'm expecting " ++ extraGenericsLength ++ " more generics here:"
                , showLocation src types
                , "The missing generics are:\n"
                , List.foldl
                    (\( name, traits ) str ->
                        str ++ name.value ++ " : " ++ (String.join " + " <| Dict.keys traits) ++ "\n"
                    )
                    ""
                    generics
                , showHintIntro
                , "1. Add the missing " ++ extraGenericsLength ++ " generics."
                , "2. Reduce the generics by " ++ extraGenericsLength ++ " to fit your types."
                ]

            -- MissingTraits _ _
            -- ExtraGenerics _
            -- UnexpectedGenericTypesInPathSegment _ _#
            _ ->
                []


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

        NamedType n generics ->
            n.value
                ++ (case generics.value of
                        [] ->
                            ""

                        _ ->
                            "<" ++ (String.join ", " <| List.map showType generics.value) ++ ">"
                   )

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

        FunctionType f ->
            showFunctionHeader f

        ArrayType ->
            "Array"

        SelfType ->
            "Self"

        GenericType name traits ->
            let
                traitNames =
                    Dict.keys traits
            in
            "Generic "
                ++ name.value
                ++ (if List.isEmpty traitNames then
                        ""

                    else
                        " : "
                            ++ String.join " + " traitNames
                   )


showFunctionHeader : FunctionHeader a -> String
showFunctionHeader { name, parameters, returnType } =
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
