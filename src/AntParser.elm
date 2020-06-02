module AntParser exposing
    ( Accessor(..)
    , CleanExpr(..)
    , CleanStmt(..)
    , Context(..)
    , Decl(..)
    , Expr(..)
    , Literal(..)
    , ArithmeticOp(..)
    , Pattern(..)
    , Problem(..)
    , Stmt(..)
    , Type(..)
    , cleanExpr
    , cleanExprs
    , cleanStmt
    , cleanStmts
    , parseExpr
    , parseStmts
    , showDeadEnds
    )

import Dict exposing (Dict)
import List.Extra
import Location exposing (Located, showProblemLocation)
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
        { name : Located String
        , accessors : List Accessor
        }
    | BlockExpr (Located Block)


type Stmt
    = VarStmt
        { target : Located Pattern
        , value : Located Expr
        }
    | LetStmt
        { target : Located Expr
        , value : Located Expr
        }
    | CallStmt
        { caller : Located Expr
        , arguments : List (Located Expr)
        }
    | ReturnStmt (Located Expr)


type Decl
    = StructDecl
        { name : Located String
        , mappings : Dict String ( Located String, Located Type )
        }
    | FunctionDecl
        { name : Located String
        , parameters : List ( Located String, Located Type )
        , returnType : Located Type
        , body : Located Block
        }


type Type
    = IntType
    | BoolType
    | CharType
    | UnitType


type Pattern
    = IdentifierPattern
        { mutable : Bool
        , name : Located String
        }
    | WildcardPattern


type Literal
    = CharLiteral Char
    | StringLiteral String
    | IntLiteral Int
    | BoolLiteral Bool
    | StructLiteral
        { name : Located String
        , mappings : Dict String ( Located String, Located Expr )
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
    | LessThanOP
    | GreaterThanOrEqualOp
    | LessThanOrEqualOp


type BooleanOp
    = BooleanAndOp
    | BooleanOrOp


type Accessor
    = ArrayAccess (Located Expr)
    | StructAccess (Located String)


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
    let
        subexpr =
            Pratt.literal << located
    in
    Pratt.expression
        { oneOf =
            List.map subexpr
                [ ifExpr
                , whileExpr
                , literalExpr
                , placeOrCallExpr
                ]
        , andThenOneOf =
            [ Pratt.infixLeft 12
                (symbol <| Token "+" <| ExpectingSymbol "+")
              <|
                formArithmeticExpr AddOp
            , Pratt.infixLeft 12
                (symbol <| Token "-" <| ExpectingSymbol "-")
              <|
                formArithmeticExpr SubtractOp
            , Pratt.infixLeft 13
                (symbol <| Token "*" <| ExpectingSymbol "*")
              <|
                formArithmeticExpr MultiplyOp
            , Pratt.infixLeft 13
                (symbol <| Token "/" <| ExpectingSymbol "/")
              <|
                formArithmeticExpr DivideOp
            , Pratt.infixLeft 10
                (symbol <| Token "&" <| ExpectingSymbol "&")
              <|
                formArithmeticExpr BitwiseAndOp
            , Pratt.infixLeft 8
                (symbol <| Token "|" <| ExpectingSymbol "|")
              <|
                formArithmeticExpr BitwiseOrOp
            ]
        , spaces = sps
        }


formArithmeticExpr : ArithmeticOp -> Located Expr -> Located Expr -> Located Expr
formArithmeticExpr op left right =
    { from =
        left.from
    , to =
        right.to
    , value =
        ArithmeticExpr
            { left = left
            , op = op
            , right = right
            }
    }


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
                    , map BlockExpr <| located block
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
                , map IntLiteral <| int ExpectingInt InvalidNumber
                , succeed
                    (\name mappings ->
                        StructLiteral
                            { name =
                                name
                            , mappings =
                                Dict.fromList <|
                                    List.map
                                        (\( key, value ) ->
                                            ( key.value, ( key, value ) )
                                        )
                                        mappings
                            }
                    )
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
                                |. symbol (Token "=" <| ExpectingSymbol "=")
                                |. sps
                                |= lazy (\_ -> expr)
                        , trailing = Optional
                        }
                ]


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


placeOrCallExpr : AntParser Expr
placeOrCallExpr =
    succeed
        (\place arguments ->
            case arguments of
                Nothing ->
                    place.value

                Just args ->
                    CallExpr
                        { caller = place
                        , arguments = args
                        }
        )
        |= located placeExpr
        |= (optional <|
                succeed identity
                    |. sps
                    |= exprList
           )


placeExpr : AntParser Expr
placeExpr =
    succeed (\name accessors -> PlaceExpr { name = name, accessors = accessors })
        |= oneOf
            [ varName ExpectingVariableName
            , tyName ExpectingStructName
            ]
        |= loop []
            (\revAccessors ->
                oneOf
                    [ succeed (\accessor -> Loop <| ArrayAccess accessor :: revAccessors)
                        |. symbol (Token "[" <| ExpectingStartOfArrayAccess)
                        |. sps
                        |= lazy (\_ -> expr)
                        |. sps
                        |. symbol (Token "]" <| ExpectingEndOfArrayAccess)
                    , succeed (\accessor -> Loop <| StructAccess accessor :: revAccessors)
                        |. symbol (Token "." <| ExpectingStartOfStructAccess)
                        |= varName ExpectingStructField
                    , succeed ()
                        |> map (\_ -> Done <| List.reverse revAccessors)
                    ]
            )


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
                    |= located placeExpr
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
                    (\caller arguments ->
                        CallStmt
                            { caller = caller
                            , arguments = arguments
                            }
                    )
                    |= located placeExpr
                    |. sps
                    |= exprList
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
        { name : String
        , accessors : List Accessor
        }
    | CArrayIndexExpr
        { array : CleanExpr
        , index : CleanExpr
        }
    | CBlockExpr CleanBlock


type CleanStmt
    = CVarStmt
        { target : Pattern
        , value : CleanExpr
        }
    | CLetStmt
        { target : CleanExpr
        , value : CleanExpr
        }
    | CCallStmt
        { caller : CleanExpr
        , arguments : List CleanExpr
        }
    | CReturnStmt CleanExpr


type alias CleanBlock =
    ( List CleanStmt, Maybe CleanExpr )


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

        PlaceExpr { name, accessors } ->
            CPlaceExpr
                { name =
                    name.value
                , accessors =
                    accessors
                }

        BlockExpr b ->
            CBlockExpr <|
                cleanBlock b


cleanExprs : List (Located Expr) -> List CleanExpr
cleanExprs es =
    List.map cleanExpr es


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
                { target = cleanExpr target
                , value = cleanExpr value
                }

        CallStmt { caller, arguments } ->
            CCallStmt
                { caller = cleanExpr caller
                , arguments = cleanExprs arguments
                }

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
