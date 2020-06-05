module AntParser exposing
    ( Accessor(..)
    , ArithmeticOp(..)
    , Block
    , BooleanOp(..)
    , CleanAccessor(..)
    , CleanDecl(..)
    , CleanExpr(..)
    , CleanPath
    , CleanPathSegment(..)
    , CleanPlace
    , CleanStmt(..)
    , ComparisonOp(..)
    , Context(..)
    , Decl(..)
    , Expr(..)
    , FunctionDecl
    , Literal(..)
    , Path
    , PathSegment(..)
    , Pattern(..)
    , Problem(..)
    , Stmt(..)
    , Type(..)
    , Variable
    , cleanDecl
    , cleanExpr
    , cleanExprs
    , cleanStmt
    , cleanStmts
    , parseDecl
    , parseDecls
    , parseExpr
    , parseStmts
    , showDeadEnds
    )

import Dict exposing (Dict)
import List.Extra
import Location exposing (Located, dummyLocated, showProblemLocation)
import Parser.Advanced exposing (..)
import Pratt.Advanced as Pratt
import Set exposing (Set)


type Expr
    = IfExpr
        { condition : Located Expr
        , thenBody : Located Block
        , elseBody : Located Expr
        }
    | WhileExpr
        { condition : Located Expr
        , body : Located Block
        }
    | LiteralExpr (Located Literal)
    | ArithmeticExpr
        { left : Located Expr
        , op : ArithmeticOp
        , right : Located Expr
        }
    | ComparisonExpr
        { left : Located Expr
        , op : ComparisonOp
        , right : Located Expr
        }
    | BooleanExpr
        { left : Located Expr
        , op : BooleanOp
        , right : Located Expr
        }
    | CallExpr
        { caller : Located Expr
        , arguments : List (Located Expr)
        }
    | PlaceExpr
        { target : Located Expr
        , accessor : Accessor
        }
    | BlockExpr Block
    | PathExpr Path


type Stmt
    = VarStmt
        { target : Located Pattern
        , value : Located Expr
        }
    | LetStmt
        { target : Located Place
        , value : Located Expr
        }
    | CallStmt (Located Expr)
    | ReturnStmt (Located Expr)


type Decl
    = StructDecl
        { name : Located String
        , fields : Dict String ( Located String, Located Type )
        }
    | FnDecl FunctionDecl
    | ImplDecl
        { target : Located String
        , functions : List FunctionDecl
        }


type alias FunctionDecl =
    { name : Located String
    , namespace : Maybe (Located String)
    , parameters : List ( Located Pattern, Located Type )
    , returnType : Located Type
    , body : Located Block
    }


type alias Place =
    { name : Located String
    , accessors : List Accessor
    }


type Type
    = IntType
    | BoolType
    | CharType
    | StringType
    | UnitType
    | NamedType (Located String)
    | StructType
        { name : Located String
        , fields : Dict String ( Located String, Located Type )
        }
    | FunctionType FunctionDecl
    | ArrayType


type Pattern
    = IdentifierPattern Variable
    | WildcardPattern


type alias Variable =
    { mutable : Bool
    , name : Located String
    }


type Literal
    = CharLiteral Char
    | StringLiteral String
    | IntLiteral Int
    | BoolLiteral Bool
    | StructLiteral
        { name : Located String
        , fields : Dict String ( Located String, Located Expr )
        }


type ArithmeticOp
    = AddOp
    | SubtractOp
    | MultiplyOp
    | DivideOp
    | BitwiseAndOp
    | BitwiseOrOp


type ComparisonOp
    = EqualOp
    | NotEqualOp
    | GreaterThanOp
    | LessThanOp
    | GreaterThanOrEqualOp
    | LessThanOrEqualOp


type BooleanOp
    = BooleanAndOp
    | BooleanOrOp


type Accessor
    = ArrayAccess (Located Expr)
    | StructAccess (Located String)


type alias Path =
    ( Located PathSegment, List (Located PathSegment) )


type PathSegment
    = IdentifierSegment (Located String)


type alias Block =
    ( List Stmt, Maybe (Located Expr) )


type alias AntParser a =
    Parser Context Problem a


type Context
    = VarContext
    | LetContext
    | CallContext
    | ReturnContext


type Problem
    = ExpectingStartOfLineComment
    | ExpectingStartOfMultiLineComment
    | ExpectingEndOfMultiLineComment
    | ExpectingStartOfBlock
    | ExpectingEndOfBlock
    | ExpectingStartOfChar
    | ExpectingEndOfChar
    | ExpectingStartOfString
    | ExpectingEndOfString
    | ExpectingChar
    | ExpectingInt
    | InvalidNumber
    | ExpectingStartOfStruct
    | ExpectingEndOfStruct
    | ExpectingVariableName
    | ExpectingStructName
    | ExpectingWildcard
    | ExpectingStructField
    | ExpectingStartOfArrayAccess
    | ExpectingEndOfArrayAccess
    | ExpectingStartOfStructAccess
    | ExpectingKeyword String
    | ExpectingSymbol String
    | ExpectingEOF
    | ExpectingType
    | ExpectingFunctionName
    | ExpectingPathSegment


reserved : Set String
reserved =
    Set.fromList
        [ "true"
        , "false"
        , "if"
        , "else"
        , "while"
        , "mut"
        ]


parseDecls : String -> Result (List (DeadEnd Context Problem)) (List Decl)
parseDecls src =
    run
        (succeed identity
            |. sps
            |= loop []
                (\revDecls ->
                    oneOf
                        [ succeed (\s -> Loop <| s :: revDecls)
                            |= decl
                            |. sps
                        , succeed ()
                            |> map (\_ -> Done <| List.reverse revDecls)
                        ]
                )
            |. end ExpectingEOF
        )
        src


parseDecl : String -> Result (List (DeadEnd Context Problem)) Decl
parseDecl src =
    run
        (succeed identity
            |= decl
            |. end ExpectingEOF
        )
        src


decl : AntParser Decl
decl =
    oneOf
        [ fnDecl
        , structDecl
        , implDecl
        ]


structDecl : AntParser Decl
structDecl =
    succeed
        (\name fields ->
            StructDecl
                { name = name
                , fields =
                    Dict.fromList <|
                        List.map
                            (\( key, value ) ->
                                ( key.value, ( key, value ) )
                            )
                            fields
                }
        )
        |. keyword (Token "struct" <| ExpectingKeyword "struct")
        |. sps
        |= tyName ExpectingStructName
        |. sps
        |= sequence
            { start = Token "{" ExpectingStartOfStruct
            , separator = Token "," <| ExpectingSymbol ","
            , end = Token "}" ExpectingEndOfStruct
            , spaces = sps
            , item =
                succeed Tuple.pair
                    |= varName ExpectingStructField
                    |. sps
                    |. symbol (Token ":" <| ExpectingSymbol ":")
                    |. sps
                    |= located ty
            , trailing = Optional
            }


fnDecl : AntParser Decl
fnDecl =
    map FnDecl <| functionDecl Nothing


functionDecl : Maybe (Located String) -> AntParser FunctionDecl
functionDecl namespace =
    succeed
        (\name parameters returnType body ->
            { name = name
            , namespace = namespace
            , parameters = parameters
            , returnType = returnType
            , body = body
            }
        )
        |. keyword (Token "fn" <| ExpectingKeyword "fn")
        |. sps
        |= varName ExpectingFunctionName
        |. sps
        |= sequence
            { start = Token "(" <| ExpectingSymbol "("
            , separator = Token "," <| ExpectingSymbol ","
            , end = Token ")" <| ExpectingSymbol ")"
            , spaces = sps
            , item =
                succeed Tuple.pair
                    |= located pattern
                    |. sps
                    |. symbol (Token ":" <| ExpectingSymbol ":")
                    |. sps
                    |= located ty
            , trailing = Optional
            }
        |. sps
        |. symbol (Token ":" <| ExpectingSymbol ":")
        |. sps
        |= located ty
        |. sps
        |= located block


implDecl : AntParser Decl
implDecl =
    succeed identity
        |. keyword (Token "impl" <| ExpectingKeyword "impl")
        |. sps
        |= tyName ExpectingStructName
        |. sps
        |> andThen
            (\target ->
                succeed
                    (\functions ->
                        ImplDecl
                            { target = target
                            , functions = functions
                            }
                    )
                    |= sequence
                        { start = Token "{" <| ExpectingSymbol "{"
                        , separator = Token "" <| ExpectingSymbol ""
                        , end = Token "}" <| ExpectingSymbol "}"
                        , spaces = sps
                        , item = functionDecl (Just target)
                        , trailing = Optional
                        }
            )


ty : AntParser Type
ty =
    oneOf
        [ map (\_ -> IntType) <| keyword (Token "Int" <| ExpectingType)
        , map (\_ -> CharType) <| keyword (Token "Char" <| ExpectingType)
        , map (\_ -> StringType) <| keyword (Token "String" <| ExpectingType)
        , map (\_ -> BoolType) <| keyword (Token "Bool" <| ExpectingType)
        , map (\_ -> UnitType) <| symbol (Token "()" <| ExpectingType)
        , map (\name -> NamedType name) <| tyName ExpectingType
        ]


parseStmts : String -> Result (List (DeadEnd Context Problem)) (List Stmt)
parseStmts src =
    run
        (succeed identity
            |. sps
            |= loop []
                (\revStmts ->
                    oneOf
                        [ succeed (\s -> Loop <| s :: revStmts)
                            |= stmt
                            |. sps
                        , succeed ()
                            |> map (\_ -> Done <| List.reverse revStmts)
                        ]
                )
            |. end ExpectingEOF
        )
        src


parseExpr : String -> Result (List (DeadEnd Context Problem)) Expr
parseExpr src =
    run
        (succeed identity
            |= map .value expr
            |. end ExpectingEOF
        )
        src


expr : AntParser (Located Expr)
expr =
    Pratt.expression
        { oneOf =
            [ Pratt.literal subExpr ]
        , andThenOneOf =
            {- arithmetic operators -}
            [ Pratt.infixLeft 13
                (symbol <| Token "*" <| ExpectingSymbol "*")
              <|
                formArithmeticExpr MultiplyOp
            , Pratt.infixLeft 13
                (symbol <| Token "/" <| ExpectingSymbol "/")
              <|
                formArithmeticExpr DivideOp
            , Pratt.infixLeft 12
                (symbol <| Token "+" <| ExpectingSymbol "+")
              <|
                formArithmeticExpr AddOp
            , Pratt.infixLeft 12
                (symbol <| Token "-" <| ExpectingSymbol "-")
              <|
                formArithmeticExpr SubtractOp
            , Pratt.infixLeft 10
                (backtrackable <| symbol <| Token "&" <| ExpectingSymbol "&")
              <|
                formArithmeticExpr BitwiseAndOp
            , Pratt.infixLeft 8
                (backtrackable <| symbol <| Token "|" <| ExpectingSymbol "|")
              <|
                formArithmeticExpr BitwiseOrOp

            {- comparison operators -}
            , Pratt.infixLeft 7
                (symbol <| Token "==" <| ExpectingSymbol "==")
              <|
                formComparisonExpr EqualOp
            , Pratt.infixLeft 7
                (symbol <| Token "!=" <| ExpectingSymbol "!=")
              <|
                formComparisonExpr NotEqualOp
            , Pratt.infixLeft 7
                (symbol <| Token ">=" <| ExpectingSymbol ">=")
              <|
                formComparisonExpr GreaterThanOrEqualOp
            , Pratt.infixLeft 7
                (symbol <| Token "<=" <| ExpectingSymbol "<=")
              <|
                formComparisonExpr LessThanOrEqualOp
            , Pratt.infixLeft 7
                (symbol <| Token ">" <| ExpectingSymbol ">")
              <|
                formComparisonExpr GreaterThanOp
            , Pratt.infixLeft 7
                (symbol <| Token "<" <| ExpectingSymbol "<")
              <|
                formComparisonExpr LessThanOp

            {- boolean operators -}
            , Pratt.infixLeft 6
                (symbol <| Token "&&" <| ExpectingSymbol "&&")
              <|
                formBooleanExpr BooleanAndOp
            , Pratt.infixLeft 5
                (symbol <| Token "||" <| ExpectingSymbol "||")
              <|
                formBooleanExpr BooleanOrOp
            ]
        , spaces = sps
        }


subExpr : AntParser (Located Expr)
subExpr =
    succeed identity
        |= (located <|
                oneOf
                    [ ifExpr
                    , whileExpr
                    , literalExpr
                    , pathExpr
                    ]
           )
        |. sps
        |> andThen
            (\e1 ->
                loop e1
                    (\e2 ->
                        oneOf
                            [ map Loop <|
                                oneOf
                                    [ map
                                        (\arguments ->
                                            { from =
                                                e1.from
                                            , to =
                                                arguments.to
                                            , value =
                                                CallExpr { caller = e2, arguments = arguments.value }
                                            }
                                        )
                                        (located exprList)
                                    , map
                                        (\accessor ->
                                            { from =
                                                e1.from
                                            , to =
                                                accessor.to
                                            , value =
                                                PlaceExpr { target = e2, accessor = accessor.value }
                                            }
                                        )
                                        (located parseArrayAccess)
                                    , map
                                        (\accessor ->
                                            { from =
                                                e1.from
                                            , to =
                                                accessor.to
                                            , value =
                                                PlaceExpr { target = e2, accessor = accessor.value }
                                            }
                                        )
                                        (located parseStructAccess)
                                    ]
                            , succeed ()
                                |> map (\_ -> Done e2)
                            ]
                    )
            )


formBooleanExpr : BooleanOp -> Located Expr -> Located Expr -> Located Expr
formBooleanExpr =
    formBinaryExpr BooleanExpr


formComparisonExpr : ComparisonOp -> Located Expr -> Located Expr -> Located Expr
formComparisonExpr =
    formBinaryExpr ComparisonExpr


formArithmeticExpr : ArithmeticOp -> Located Expr -> Located Expr -> Located Expr
formArithmeticExpr =
    formBinaryExpr ArithmeticExpr


formBinaryExpr :
    ({ left : Located Expr, op : a, right : Located Expr } -> Expr)
    -> a
    -> Located Expr
    -> Located Expr
    -> Located Expr
formBinaryExpr f op left right =
    { from =
        left.from
    , to =
        right.to
    , value =
        f
            { left = left
            , op = op
            , right = right
            }
    }


pathExpr : AntParser Expr
pathExpr =
    map PathExpr <|
        succeed Tuple.pair
            |= located pathSegment
            |= loop []
                (\revSegments ->
                    oneOf
                        [ succeed (\segment -> Loop <| segment :: revSegments)
                            |. symbol (Token "::" <| ExpectingSymbol "::")
                            |= located pathSegment
                        , succeed ()
                            |> map (\_ -> Done <| List.reverse revSegments)
                        ]
                )


pathSegment : AntParser PathSegment
pathSegment =
    oneOf
        [ map IdentifierSegment <|
            located <|
                variable
                    { start = Char.isAlpha
                    , inner = Char.isAlphaNum
                    , reserved = reserved
                    , expecting = ExpectingPathSegment
                    }
        ]


ifExpr : AntParser Expr
ifExpr =
    succeed
        (\condition thenBody elseBody ->
            IfExpr
                { condition = condition
                , thenBody = thenBody
                , elseBody = elseBody
                }
        )
        |. keyword (Token "if" <| ExpectingKeyword "if")
        |. sps
        |= lazy (\_ -> expr)
        |. sps
        |= located block
        |. sps
        |. keyword (Token "else" <| ExpectingKeyword "else")
        |. sps
        |= (located <|
                oneOf
                    [ lazy (\_ -> ifExpr)
                    , map BlockExpr block
                    ]
           )


whileExpr : AntParser Expr
whileExpr =
    succeed
        (\condition body ->
            WhileExpr
                { condition = condition
                , body = body
                }
        )
        |. keyword (Token "while" <| ExpectingKeyword "while")
        |. sps
        |= lazy (\_ -> expr)
        |. sps
        |= located block


literalExpr : AntParser Expr
literalExpr =
    map LiteralExpr <|
        located <|
            oneOf
                [ succeed
                    (\str ->
                        CharLiteral <|
                            case String.uncons str of
                                Just ( c, _ ) ->
                                    c

                                Nothing ->
                                    '?'
                     -- impossible
                    )
                    |. symbol (Token "'" <| ExpectingStartOfChar)
                    |= (getChompedString <| chompIf (\_ -> True) ExpectingChar)
                    |. symbol (Token "'" <| ExpectingEndOfChar)
                , succeed StringLiteral
                    |. symbol (Token "\"" <| ExpectingStartOfString)
                    |= (getChompedString <| chompWhile (\c -> c /= '"'))
                    |. symbol (Token "\"" <| ExpectingEndOfString)
                , oneOf
                    [ map (\_ -> BoolLiteral True) <| keyword (Token "true" <| ExpectingKeyword "true")
                    , map (\_ -> BoolLiteral False) <| keyword (Token "false" <| ExpectingKeyword "true")
                    ]
                , map IntLiteral integer
                , succeed
                    (\name fields ->
                        StructLiteral
                            { name =
                                name
                            , fields =
                                Dict.fromList <|
                                    List.map
                                        (\( key, value ) ->
                                            ( key.value, ( key, value ) )
                                        )
                                        fields
                            }
                    )
                    |= (backtrackable <| tyName ExpectingStructName)
                    |. backtrackable sps
                    |= sequence
                        { start = Token "{" ExpectingStartOfStruct
                        , separator = Token "," <| ExpectingSymbol ","
                        , end = Token "}" ExpectingEndOfStruct
                        , spaces = sps
                        , item =
                            succeed Tuple.pair
                                |= varName ExpectingStructField
                                |. sps
                                |. symbol (Token "=" <| ExpectingSymbol "=")
                                |. sps
                                |= lazy (\_ -> expr)
                        , trailing = Optional
                        }
                ]


integer : AntParser Int
integer =
    succeed
        (\sign value ->
            case sign of
                Just _ ->
                    negate value

                Nothing ->
                    value
        )
        |= optional (symbol <| Token "-" <| ExpectingSymbol "-")
        |= (backtrackable <|
                int ExpectingInt ExpectingInt
           )


pattern : AntParser Pattern
pattern =
    oneOf
        [ succeed
            (\mutable name ->
                IdentifierPattern
                    { mutable = mutable
                    , name = name
                    }
            )
            |= optionalWithDefault
                False
                (map (\_ -> True) <| keyword <| Token "mut" <| ExpectingKeyword "mut")
            |. sps
            |= varName ExpectingVariableName
        , succeed WildcardPattern
            |. symbol (Token "_" <| ExpectingWildcard)
        ]


place : AntParser Place
place =
    succeed (\name accessors -> { name = name, accessors = accessors })
        |= varName ExpectingVariableName
        |= parseAccessors


parseAccessors : AntParser (List Accessor)
parseAccessors =
    loop []
        (\revAccessors ->
            oneOf
                [ succeed (\accessor -> Loop <| accessor :: revAccessors)
                    |= parseArrayAccess
                , succeed (\accessor -> Loop <| accessor :: revAccessors)
                    |= parseStructAccess
                , succeed ()
                    |> map (\_ -> Done <| List.reverse revAccessors)
                ]
        )


parseArrayAccess : AntParser Accessor
parseArrayAccess =
    succeed ArrayAccess
        |. symbol (Token "[" <| ExpectingStartOfArrayAccess)
        |. sps
        |= lazy (\_ -> expr)
        |. sps
        |. symbol (Token "]" <| ExpectingEndOfArrayAccess)


parseStructAccess : AntParser Accessor
parseStructAccess =
    succeed StructAccess
        |. symbol (Token "." <| ExpectingStartOfStructAccess)
        |= varName ExpectingStructField


stmt : AntParser Stmt
stmt =
    oneOf
        [ varStmt
        , letStmt
        , callStmt
        , returnStmt
        ]


varStmt : AntParser Stmt
varStmt =
    succeed identity
        |. keyword (Token "var" <| ExpectingKeyword "var")
        |. sps
        |= (inContext VarContext <|
                succeed
                    (\target value ->
                        VarStmt
                            { target = target
                            , value = value
                            }
                    )
                    |= located pattern
                    |. sps
                    |. symbol (Token "=" <| ExpectingSymbol "=")
                    |. sps
                    |= expr
                    |. sps
                    |. symbol (Token ";" <| ExpectingSymbol ";")
           )


letStmt : AntParser Stmt
letStmt =
    succeed identity
        |. keyword (Token "let" <| ExpectingKeyword "let")
        |. sps
        |= (inContext LetContext <|
                succeed
                    (\target value ->
                        LetStmt
                            { target = target
                            , value = value
                            }
                    )
                    |= located place
                    |. sps
                    |. symbol (Token "=" <| ExpectingSymbol "=")
                    |. sps
                    |= expr
                    |. sps
                    |. symbol (Token ";" <| ExpectingSymbol ";")
           )


callStmt : AntParser Stmt
callStmt =
    succeed identity
        |. keyword (Token "call" <| ExpectingKeyword "call")
        |. sps
        |= (inContext CallContext <|
                succeed
                    (\e ->
                        CallStmt e
                    )
                    |= expr
                    |. sps
                    |. symbol (Token ";" <| ExpectingSymbol ";")
           )


returnStmt : AntParser Stmt
returnStmt =
    succeed identity
        |. keyword (Token "return" <| ExpectingKeyword "return")
        |. sps
        |= (inContext ReturnContext <|
                succeed ReturnStmt
                    |= lazy (\_ -> expr)
           )


exprList : AntParser (List (Located Expr))
exprList =
    sequence
        { start = Token "(" <| ExpectingSymbol "("
        , separator = Token "," <| ExpectingSymbol ","
        , end = Token ")" <| ExpectingSymbol ")"
        , spaces = sps
        , item = lazy (\_ -> expr)
        , trailing = Optional
        }


block : AntParser Block
block =
    succeed identity
        |. symbol (Token "{" <| ExpectingStartOfBlock)
        |. sps
        |= loop []
            (\revStmts ->
                oneOf
                    [ succeed (\s -> Loop <| s :: revStmts)
                        |= stmt
                        |. sps
                    , succeed (\e -> Done <| ( List.reverse revStmts, e ))
                        |= (optional <| expr)
                    ]
            )
        |. sps
        |. symbol (Token "}" <| ExpectingEndOfBlock)


varName : Problem -> AntParser (Located String)
varName expecting =
    located <|
        variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = reserved
            , expecting = expecting
            }


tyName : Problem -> AntParser (Located String)
tyName expecting =
    located <|
        variable
            { start = Char.isUpper
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            , expecting = expecting
            }


sps : AntParser ()
sps =
    loop 0 <|
        ifProgress <|
            oneOf
                [ succeed () |. symbol (Token "--" ExpectingStartOfLineComment) |. chompWhile (\c -> c /= '\n')
                , multiComment (Token "{-" ExpectingStartOfMultiLineComment) (Token "-}" ExpectingEndOfMultiLineComment) Nestable
                , spaces
                ]


ifProgress : AntParser a -> Int -> AntParser (Step Int ())
ifProgress parser offset =
    succeed identity
        |. parser
        |= getOffset
        |> map
            (\newOffset ->
                if offset == newOffset then
                    Done ()

                else
                    Loop newOffset
            )


located : AntParser a -> AntParser (Located a)
located parser =
    succeed Located
        |= getPosition
        |= parser
        |= getPosition


optional : AntParser a -> AntParser (Maybe a)
optional parser =
    oneOf
        [ backtrackable parser |> map Just
        , succeed Nothing
        ]


optionalWithDefault : a -> AntParser a -> AntParser a
optionalWithDefault default parser =
    oneOf
        [ backtrackable parser
        , succeed default
        ]


showProjectDeadEnds : Dict String String -> Dict String (List (DeadEnd Context Problem)) -> String
showProjectDeadEnds projectSources projectDeadEnds =
    Dict.foldl
        (\programName deadEnds str ->
            case Dict.get programName projectSources of
                Just src ->
                    "-- PARSE ERROR in "
                        ++ programName
                        ++ ".vm\n\n"
                        ++ showDeadEnds src deadEnds
                        ++ "\n\n"
                        ++ str

                Nothing ->
                    str
        )
        ""
        projectDeadEnds


showDeadEnds : String -> List (DeadEnd Context Problem) -> String
showDeadEnds src deadEnds =
    let
        deadEndGroups =
            List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
    in
    String.join "\n" <| List.map (showDeadEndsHelper src) deadEndGroups


showDeadEndsHelper : String -> ( DeadEnd Context Problem, List (DeadEnd Context Problem) ) -> String
showDeadEndsHelper src ( first, rests ) =
    let
        location =
            showProblemLocation first.row first.col src

        context =
            showProblemContextStack first.contextStack
    in
    location
        ++ "\n"
        ++ String.join "\n"
            (case first.problem of
                InvalidNumber ->
                    [ "I found an invalid number."
                    , "Hint: Change it to a decimal integer."
                    ]

                _ ->
                    let
                        problemStrs =
                            List.map (.problem >> showProblem) <| List.reverse <| first :: rests
                    in
                    [ "I'm expecting " ++ String.join " or " problemStrs
                    ]
            )
        ++ (if String.isEmpty context then
                ""

            else
                " in the " ++ context ++ "."
           )


showProblem : Problem -> String
showProblem p =
    case p of
        ExpectingStartOfLineComment ->
            "start of line comment '--'"

        ExpectingStartOfMultiLineComment ->
            "start of line comment '{-'"

        ExpectingEndOfMultiLineComment ->
            "end of line comment '-}'"

        ExpectingStartOfBlock ->
            "start of block '{'"

        ExpectingEndOfBlock ->
            "end of block '}'"

        ExpectingStartOfChar ->
            "start of character literal \"'\""

        ExpectingEndOfChar ->
            "end of character literal \"'\""

        ExpectingStartOfString ->
            "start of string literal '\"'"

        ExpectingEndOfString ->
            "end of string literal '\"'"

        ExpectingChar ->
            "a character"

        ExpectingInt ->
            "an integer"

        InvalidNumber ->
            "a valid number"

        ExpectingStartOfStruct ->
            "start of struct '{'"

        ExpectingEndOfStruct ->
            "end of struct '}'"

        ExpectingVariableName ->
            "a variable name"

        ExpectingStructName ->
            "a struct name"

        ExpectingWildcard ->
            "a wildcard '_'"

        ExpectingStructField ->
            "a struct field"

        ExpectingStartOfArrayAccess ->
            "start of array access '['"

        ExpectingEndOfArrayAccess ->
            "end of array access ']'"

        ExpectingStartOfStructAccess ->
            "start of struct access '.'"

        ExpectingKeyword keyword ->
            "keyword '" ++ keyword ++ "'"

        ExpectingSymbol symbol ->
            "symbol '" ++ symbol ++ "'"

        ExpectingEOF ->
            "end of program"

        ExpectingType ->
            "a type"

        ExpectingFunctionName ->
            "a function name"

        ExpectingPathSegment ->
            "a path segment"


showProblemContextStack : List { row : Int, col : Int, context : Context } -> String
showProblemContextStack contexts =
    String.join " of the " <| List.map (.context >> showProblemContext) contexts


showProblemContext : Context -> String
showProblemContext context =
    case context of
        VarContext ->
            "variable declaration"

        LetContext ->
            "assignment statement"

        CallContext ->
            "call statement"

        ReturnContext ->
            "return statement"


type CleanExpr
    = CIfExpr
        { condition : CleanExpr
        , thenBody : CleanBlock
        , elseBody : CleanExpr
        }
    | CWhileExpr
        { condition : CleanExpr
        , body : CleanBlock
        }
    | CLiteralExpr Literal
    | CArithmeticExpr
        { left : CleanExpr
        , op : ArithmeticOp
        , right : CleanExpr
        }
    | CComparisonExpr
        { left : CleanExpr
        , op : ComparisonOp
        , right : CleanExpr
        }
    | CBooleanExpr
        { left : CleanExpr
        , op : BooleanOp
        , right : CleanExpr
        }
    | CCallExpr
        { caller : CleanExpr
        , arguments : List CleanExpr
        }
    | CPlaceExpr
        { target : CleanExpr
        , accessor : CleanAccessor
        }
    | CArrayIndexExpr
        { array : CleanExpr
        , index : CleanExpr
        }
    | CBlockExpr CleanBlock
    | CPathExpr CleanPath


type CleanStmt
    = CVarStmt
        { target : Pattern
        , value : CleanExpr
        }
    | CLetStmt
        { target : CleanPlace
        , value : CleanExpr
        }
    | CCallStmt CleanExpr
    | CReturnStmt CleanExpr


type CleanDecl
    = CStructDecl
        { name : String
        , fields : Dict String Type
        }
    | CFnDecl CFunctionDecl
    | CImplDecl
        { target : String
        , functions : List CFunctionDecl
        }


type alias CFunctionDecl =
    { name : String
    , parameters : List ( Pattern, Type )
    , returnType : Type
    , body : CleanBlock
    }


type alias CleanBlock =
    ( List CleanStmt, Maybe CleanExpr )


type alias CleanPlace =
    { name : String
    , accessors : List CleanAccessor
    }


type CleanAccessor
    = CArrayAccess CleanExpr
    | CStructAccess String


type alias CleanPath =
    ( CleanPathSegment, List CleanPathSegment )


type CleanPathSegment
    = CIdentifierSegment String


cleanExpr : Located Expr -> CleanExpr
cleanExpr e =
    case e.value of
        IfExpr { condition, thenBody, elseBody } ->
            CIfExpr
                { condition =
                    cleanExpr condition
                , thenBody =
                    cleanBlock thenBody
                , elseBody =
                    cleanExpr elseBody
                }

        WhileExpr { condition, body } ->
            CWhileExpr
                { condition =
                    cleanExpr condition
                , body =
                    cleanBlock body
                }

        LiteralExpr literal ->
            CLiteralExpr literal.value

        ArithmeticExpr { left, op, right } ->
            CArithmeticExpr
                { left =
                    cleanExpr left
                , op =
                    op
                , right =
                    cleanExpr right
                }

        ComparisonExpr { left, op, right } ->
            CComparisonExpr
                { left =
                    cleanExpr left
                , op =
                    op
                , right =
                    cleanExpr right
                }

        BooleanExpr { left, op, right } ->
            CBooleanExpr
                { left =
                    cleanExpr left
                , op =
                    op
                , right =
                    cleanExpr right
                }

        CallExpr { caller, arguments } ->
            CCallExpr
                { caller =
                    cleanExpr caller
                , arguments =
                    cleanExprs arguments
                }

        PlaceExpr { target, accessor } ->
            CPlaceExpr
                { target =
                    cleanExpr target
                , accessor =
                    cleanAccessor accessor
                }

        BlockExpr b ->
            CBlockExpr <|
                cleanBlock <|
                    dummyLocated b

        PathExpr ( firstSegment, restSegments ) ->
            CPathExpr
                ( cleanPathSegment firstSegment
                , List.map cleanPathSegment restSegments
                )


cleanPathSegment : Located PathSegment -> CleanPathSegment
cleanPathSegment s =
    case s.value of
        IdentifierSegment name ->
            CIdentifierSegment name.value


cleanExprs : List (Located Expr) -> List CleanExpr
cleanExprs es =
    List.map cleanExpr es


cleanPlace : Place -> CleanPlace
cleanPlace { name, accessors } =
    { name =
        name.value
    , accessors =
        List.map cleanAccessor accessors
    }


cleanAccessor : Accessor -> CleanAccessor
cleanAccessor a =
    case a of
        ArrayAccess e ->
            CArrayAccess <| cleanExpr e

        StructAccess n ->
            CStructAccess n.value


cleanStmt : Stmt -> CleanStmt
cleanStmt s =
    case s of
        VarStmt { target, value } ->
            CVarStmt
                { target = target.value
                , value = cleanExpr value
                }

        LetStmt { target, value } ->
            CLetStmt
                { target = cleanPlace target.value
                , value = cleanExpr value
                }

        CallStmt e ->
            CCallStmt <| cleanExpr e

        ReturnStmt e ->
            CReturnStmt <| cleanExpr e


cleanStmts : List Stmt -> List CleanStmt
cleanStmts stmts =
    List.map cleanStmt stmts


cleanBlock : Located Block -> CleanBlock
cleanBlock b =
    ( cleanStmts <| Tuple.first b.value
    , Maybe.map cleanExpr <| Tuple.second b.value
    )


cleanDecl : Decl -> CleanDecl
cleanDecl d =
    case d of
        StructDecl { name, fields } ->
            CStructDecl
                { name = name.value
                , fields =
                    Dict.foldl
                        (\_ ( fieldName, fieldType ) cleanDict ->
                            Dict.insert fieldName.value fieldType.value cleanDict
                        )
                        Dict.empty
                        fields
                }

        FnDecl f ->
            CFnDecl <| cleanFunctionDecl f

        ImplDecl { target, functions } ->
            CImplDecl
                { target = target.value
                , functions =
                    List.map cleanFunctionDecl functions
                }


cleanFunctionDecl : FunctionDecl -> CFunctionDecl
cleanFunctionDecl { name, parameters, returnType, body } =
    { name = name.value
    , parameters =
        List.map
            (\( paramName, paramType ) ->
                ( paramName.value, paramType.value )
            )
            parameters
    , returnType = returnType.value
    , body =
        cleanBlock body
    }
