module AntParserTest exposing (..)

import AntParser
    exposing
        ( Accessor(..)
        , ArithmeticOp(..)
        , BooleanOp(..)
        , CleanAccessor(..)
        , CleanDecl(..)
        , CleanExpr(..)
        , CleanLiteral(..)
        , CleanNamespace(..)
        , CleanPathSegment(..)
        , CleanPattern(..)
        , CleanStmt(..)
        , CleanType(..)
        , ComparisonOp(..)
        , Context
        , Expr(..)
        , Literal(..)
        , Pattern(..)
        , Problem(..)
        , Stmt(..)
        , Type(..)
        , cleanExpr
        )
import Dict
import Expect
import Location exposing (dummyLocated)
import Parser.Advanced exposing (DeadEnd)
import Test exposing (Test, describe)


testExpr : Test
testExpr =
    let
        testWithLocation : String -> String -> Result (List (DeadEnd Context Problem)) Expr -> Test
        testWithLocation description src expected =
            Test.test
                description
                (\_ -> Expect.equal expected (AntParser.parseExpr src))

        test : String -> String -> Result (List (DeadEnd Context Problem)) CleanExpr -> Test
        test description src expected =
            Test.test
                description
                (\_ -> Expect.equal expected (Result.map (AntParser.cleanExpr << dummyLocated) <| AntParser.parseExpr src))
    in
    describe "Test Expr"
        [ describe "LiteralExpr"
            [ testWithLocation "Char"
                "'a'"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 4 ), value = CharLiteral 'a' }
            , testWithLocation "unterminated Char"
                "'a"
              <|
                Err [ { col = 3, contextStack = [], problem = ExpectingEndOfChar, row = 1 } ]
            , testWithLocation "too many Char"
                "'abc'"
              <|
                Err [ { col = 3, contextStack = [], problem = ExpectingEndOfChar, row = 1 } ]
            , testWithLocation "String"
                "\"it's a nice day!\""
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 19 ), value = StringLiteral "it's a nice day!" }
            , testWithLocation "unterminated String"
                "\"it's a nice day!"
              <|
                Err [ { col = 18, contextStack = [], problem = ExpectingEndOfString, row = 1 } ]
            , testWithLocation "Positive Int"
                "38391900"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 9 ), value = IntLiteral 38391900 }
            , testWithLocation "Zero"
                "0"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 2 ), value = IntLiteral 0 }
            , testWithLocation "Negative Int"
                "-38391900"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 10 ), value = IntLiteral -38391900 }
            , testWithLocation "Bool : true"
                "true"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 5 ), value = BoolLiteral True }
            , testWithLocation "Bool : false"
                "false"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 6 ), value = BoolLiteral False }
            , testWithLocation "struct"
                """MyStruct {
  key1 = "value1",
}"""
              <|
                Ok (LiteralExpr { from = ( 1, 1 ), to = ( 3, 2 ), value = StructLiteral { fields = Dict.fromList [ ( "key1", ( { from = ( 2, 3 ), to = ( 2, 7 ), value = "key1" }, { from = ( 2, 10 ), to = ( 2, 18 ), value = LiteralExpr { from = ( 2, 10 ), to = ( 2, 18 ), value = StringLiteral "value1" } } ) ) ], name = { from = ( 1, 1 ), to = ( 1, 9 ), value = "MyStruct" } } })
            ]
        , describe "pathExpr"
            [ test "single variable"
                "a"
              <|
                Ok (CPathExpr ( CIdentifierSegment "a", [] ))
            , test "simple path"
                "a::b::c"
              <|
                Ok (CPathExpr ( CIdentifierSegment "a", [ CIdentifierSegment "b", CIdentifierSegment "c" ] ))
            ]
        , describe "ArithmeticExpr"
            [ test "add"
                "2 + 3 + 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = AddOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        , op = AddOp
                        , right = CLiteralExpr (CIntLiteral 4)
                        }
                    )
            , test "subtract"
                "2 - 3 - 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = SubtractOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        , op = SubtractOp
                        , right = CLiteralExpr (CIntLiteral 4)
                        }
                    )
            , test "mixed add and subtract"
                "2 + 3 - 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = AddOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        , op = SubtractOp
                        , right = CLiteralExpr (CIntLiteral 4)
                        }
                    )
            , test "mixed add and subtract on positive and negative ints"
                "2 + -3 - -4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = AddOp
                                , right = CLiteralExpr (CIntLiteral -3)
                                }
                        , op = SubtractOp
                        , right = CLiteralExpr (CIntLiteral -4)
                        }
                    )
            , test "multiply"
                "2 * 3 * 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = MultiplyOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        , op = MultiplyOp
                        , right = CLiteralExpr (CIntLiteral 4)
                        }
                    )
            , test "divide"
                "2 / 3 / 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = DivideOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        , op = DivideOp
                        , right = CLiteralExpr (CIntLiteral 4)
                        }
                    )
            , test "mixed multiply and divide"
                "2 * 3 / 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = MultiplyOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        , op = DivideOp
                        , right = CLiteralExpr (CIntLiteral 4)
                        }
                    )
            , test "mixed add, subtract, multiply, and divide"
                "1 + 2 * 3 - 4 / 5"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left =
                                    CLiteralExpr (CIntLiteral 1)
                                , op = AddOp
                                , right =
                                    CArithmeticExpr
                                        { left = CLiteralExpr (CIntLiteral 2)
                                        , op = MultiplyOp
                                        , right = CLiteralExpr (CIntLiteral 3)
                                        }
                                }
                        , op =
                            SubtractOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 4)
                                , op = DivideOp
                                , right = CLiteralExpr (CIntLiteral 5)
                                }
                        }
                    )
            , test "mixed add, subtract, multiply, and divide on positive and negative ints"
                "-1 + 2 * -3 - -4 / -5"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left =
                                    CLiteralExpr (CIntLiteral -1)
                                , op = AddOp
                                , right =
                                    CArithmeticExpr
                                        { left = CLiteralExpr (CIntLiteral 2)
                                        , op = MultiplyOp
                                        , right = CLiteralExpr (CIntLiteral -3)
                                        }
                                }
                        , op =
                            SubtractOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral -4)
                                , op = DivideOp
                                , right = CLiteralExpr (CIntLiteral -5)
                                }
                        }
                    )
            , test "Bitwise AND"
                "1 & 2 & 3"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 1)
                                , op = BitwiseAndOp
                                , right = CLiteralExpr (CIntLiteral 2)
                                }
                        , op = BitwiseAndOp
                        , right = CLiteralExpr (CIntLiteral 3)
                        }
                    )
            , test "Bitwise OR"
                "1 | 2 | 3"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 1)
                                , op = BitwiseOrOp
                                , right = CLiteralExpr (CIntLiteral 2)
                                }
                        , op = BitwiseOrOp
                        , right = CLiteralExpr (CIntLiteral 3)
                        }
                    )
            , test "mixed Bitwise AND and OR"
                "1 | 2 & 3"
              <|
                Ok
                    (CArithmeticExpr
                        { left = CLiteralExpr (CIntLiteral 1)
                        , op = BitwiseOrOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 2)
                                , op = BitwiseAndOp
                                , right = CLiteralExpr (CIntLiteral 3)
                                }
                        }
                    )
            , test "mixed arithmetic operations"
                "1 & 5 + 2 | 3 / 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 1)
                                , op = BitwiseAndOp
                                , right =
                                    CArithmeticExpr
                                        { left = CLiteralExpr (CIntLiteral 5)
                                        , op = AddOp
                                        , right = CLiteralExpr (CIntLiteral 2)
                                        }
                                }
                        , op = BitwiseOrOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 3)
                                , op = DivideOp
                                , right = CLiteralExpr (CIntLiteral 4)
                                }
                        }
                    )
            ]
        , describe "ComparisonExpr"
            [ test "equal"
                "0 == 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (CIntLiteral 0)
                        , op = EqualOp
                        , right = CLiteralExpr (CIntLiteral 1)
                        }
                    )
            , test "not equal"
                "0 != 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (CIntLiteral 0)
                        , op = NotEqualOp
                        , right = CLiteralExpr (CIntLiteral 1)
                        }
                    )
            , test "greater than or equal"
                "0 >= 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (CIntLiteral 0)
                        , op = GreaterThanOrEqualOp
                        , right = CLiteralExpr (CIntLiteral 1)
                        }
                    )
            , test "less than or equal"
                "0 <= 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (CIntLiteral 0)
                        , op = LessThanOrEqualOp
                        , right = CLiteralExpr (CIntLiteral 1)
                        }
                    )
            , test "greater than"
                "0 > 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (CIntLiteral 0)
                        , op = GreaterThanOp
                        , right = CLiteralExpr (CIntLiteral 1)
                        }
                    )
            , test "less than"
                "0 < 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (CIntLiteral 0)
                        , op = LessThanOp
                        , right = CLiteralExpr (CIntLiteral 1)
                        }
                    )
            , test "mixed arithmetic and comparison operations"
                "1 + 2 < 3 * 4"
              <|
                Ok
                    (CComparisonExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 1)
                                , op = AddOp
                                , right = CLiteralExpr (CIntLiteral 2)
                                }
                        , op = LessThanOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (CIntLiteral 3)
                                , op = MultiplyOp
                                , right = CLiteralExpr (CIntLiteral 4)
                                }
                        }
                    )
            ]
        , describe "boolean operations"
            [ test "boolean AND"
                "true && false && false"
              <|
                Ok
                    (CBooleanExpr
                        { left =
                            CBooleanExpr
                                { left = CLiteralExpr (CBoolLiteral True)
                                , op = BooleanAndOp
                                , right = CLiteralExpr (CBoolLiteral False)
                                }
                        , op = BooleanAndOp
                        , right = CLiteralExpr (CBoolLiteral False)
                        }
                    )
            , test "boolean OR"
                "true || false || false"
              <|
                Ok
                    (CBooleanExpr
                        { left =
                            CBooleanExpr
                                { left = CLiteralExpr (CBoolLiteral True)
                                , op = BooleanOrOp
                                , right = CLiteralExpr (CBoolLiteral False)
                                }
                        , op = BooleanOrOp
                        , right = CLiteralExpr (CBoolLiteral False)
                        }
                    )
            ]
        , describe "IfExpr"
            [ testWithLocation "if + else"
                """if true {
  1
} else {
  0
}"""
              <|
                Ok
                    (IfExpr
                        { condition =
                            { from = ( 1, 4 ), to = ( 1, 8 ), value = LiteralExpr { from = ( 1, 4 ), to = ( 1, 8 ), value = BoolLiteral True } }
                        , thenBody =
                            { from = ( 1, 9 ), to = ( 3, 2 ), value = ( [], Just { from = ( 2, 3 ), to = ( 2, 4 ), value = LiteralExpr { from = ( 2, 3 ), to = ( 2, 4 ), value = IntLiteral 1 } } ) }
                        , elseBody =
                            { from = ( 3, 8 ), to = ( 5, 2 ), value = BlockExpr ( [], Just <| { from = ( 4, 3 ), to = ( 4, 4 ), value = LiteralExpr { from = ( 4, 3 ), to = ( 4, 4 ), value = IntLiteral 0 } } ) }
                        }
                    )
            , test "if + 1 else if + else"
                """if true {
  2
} else if false {
  1
} else {
  0
}"""
              <|
                Ok
                    (CIfExpr
                        { condition =
                            CLiteralExpr (CBoolLiteral True)
                        , elseBody =
                            CIfExpr
                                { condition =
                                    CLiteralExpr (CBoolLiteral False)
                                , elseBody = CBlockExpr ( [], Just (CLiteralExpr (CIntLiteral 0)) )
                                , thenBody = ( [], Just (CLiteralExpr (CIntLiteral 1)) )
                                }
                        , thenBody = ( [], Just (CLiteralExpr (CIntLiteral 2)) )
                        }
                    )
            , test "if + 2 else if + else"
                """if true {
  3
} else if false {
  2
} else if true {
  1
} else {
  0
}"""
              <|
                Ok
                    (CIfExpr
                        { condition =
                            CLiteralExpr (CBoolLiteral True)
                        , elseBody =
                            CIfExpr
                                { condition =
                                    CLiteralExpr (CBoolLiteral False)
                                , elseBody =
                                    CIfExpr
                                        { condition =
                                            CLiteralExpr (CBoolLiteral True)
                                        , elseBody =
                                            CBlockExpr ( [], Just (CLiteralExpr (CIntLiteral 0)) )
                                        , thenBody =
                                            ( [], Just (CLiteralExpr (CIntLiteral 1)) )
                                        }
                                , thenBody = ( [], Just (CLiteralExpr (CIntLiteral 2)) )
                                }
                        , thenBody = ( [], Just (CLiteralExpr (CIntLiteral 3)) )
                        }
                    )
            , test "expecting start of block"
                "if true false"
              <|
                Err [ { col = 9, contextStack = [], problem = ExpectingStartOfBlock, row = 1 } ]
            , test "expecting end of block"
                "if true { 0 } else { 1 "
              <|
                Err [ { col = 24, contextStack = [], problem = ExpectingEndOfBlock, row = 1 } ]
            , test "expecting else branch"
                "if true { 0 }"
              <|
                Err [ { col = 14, contextStack = [], problem = ExpectingKeyword "else", row = 1 } ]
            ]
        , describe "while and block"
            [ test "body containing several stmts"
                """while true {
    var a = 0;
    let a = 1;
}"""
              <|
                Ok
                    (CWhileExpr
                        { body =
                            ( [ CVarStmt
                                    { target =
                                        CIdentifierPattern { mutable = False, name = "a" }
                                    , value = CLiteralExpr (CIntLiteral 0)
                                    }
                              , CLetStmt
                                    { target = { accessors = [], name = "a" }
                                    , value = CLiteralExpr (CIntLiteral 1)
                                    }
                              ]
                            , Nothing
                            )
                        , condition = CLiteralExpr (CBoolLiteral True)
                        }
                    )
            , test "body containing several stmts ended with expr"
                """while true {
    var a = 0;
    let a = 1;
    a
}"""
              <|
                Ok
                    (CWhileExpr
                        { body =
                            ( [ CVarStmt
                                    { target =
                                        CIdentifierPattern { mutable = False, name = "a" }
                                    , value = CLiteralExpr (CIntLiteral 0)
                                    }
                              , CLetStmt
                                    { target = { accessors = [], name = "a" }
                                    , value = CLiteralExpr (CIntLiteral 1)
                                    }
                              ]
                            , Just <|
                                CPathExpr ( CIdentifierSegment "a", [] )
                            )
                        , condition = CLiteralExpr (CBoolLiteral True)
                        }
                    )
            , test "body containing single expr"
                """while true {
    1
}"""
              <|
                Ok
                    (CWhileExpr
                        { body =
                            ( []
                            , Just (CLiteralExpr (CIntLiteral 1))
                            )
                        , condition = CLiteralExpr (CBoolLiteral True)
                        }
                    )
            ]
        , describe "PlaceExpr"
            [ test "array access"
                "a[0]"
              <|
                Ok
                    (CPlaceExpr
                        { target = CPathExpr ( CIdentifierSegment "a", [] )
                        , accessor =
                            CArrayAccess <| CLiteralExpr (CIntLiteral 0)
                        }
                    )
            , test "struct access"
                "a.fieldName"
              <|
                Ok
                    (CPlaceExpr
                        { accessor =
                            CStructAccess "fieldName"
                        , target = CPathExpr ( CIdentifierSegment "a", [] )
                        }
                    )
            , test "string of struct accesses"
                "a.fieldName1.fieldName2"
              <|
                Ok
                    (CPlaceExpr
                        { target =
                            CPlaceExpr
                                { accessor =
                                    CStructAccess "fieldName1"
                                , target = CPathExpr ( CIdentifierSegment "a", [] )
                                }
                        , accessor =
                            CStructAccess "fieldName2"
                        }
                    )
            , test "mixed accesses"
                "a[0].fieldName1.fieldName2[1][2]"
              <|
                Ok
                    (CPlaceExpr
                        { accessor = CArrayAccess (CLiteralExpr (CIntLiteral 2))
                        , target =
                            CPlaceExpr
                                { accessor = CArrayAccess (CLiteralExpr (CIntLiteral 1))
                                , target =
                                    CPlaceExpr
                                        { accessor = CStructAccess "fieldName2"
                                        , target =
                                            CPlaceExpr
                                                { accessor = CStructAccess "fieldName1"
                                                , target =
                                                    CPlaceExpr
                                                        { accessor = CArrayAccess (CLiteralExpr (CIntLiteral 0))
                                                        , target = CPathExpr ( CIdentifierSegment "a", [] )
                                                        }
                                                }
                                        }
                                }
                        }
                    )
            , describe "CallExpr"
                [ test "no argument"
                    "a()"
                  <|
                    Ok (CCallExpr { arguments = [], caller = CPathExpr ( CIdentifierSegment "a", [] ) })
                , test "no argument with more complex path as caller"
                    "a::b::c()"
                  <|
                    Ok
                        (CCallExpr
                            { arguments = []
                            , caller = CPathExpr ( CIdentifierSegment "a", [ CIdentifierSegment "b", CIdentifierSegment "c" ] )
                            }
                        )
                , test "no argument with struct literal as caller"
                    "MyStruct { a = 0 }()"
                  <|
                    Ok
                        (CCallExpr
                            { arguments = []
                            , caller =
                                CLiteralExpr
                                    (CStructLiteral
                                        { fields =
                                            Dict.fromList
                                                [ ( "a"
                                                  , CLiteralExpr (CIntLiteral 0)
                                                  )
                                                ]
                                        , name = "MyStruct"
                                        }
                                    )
                            }
                        )
                , test "1 argument"
                    "a(0)"
                  <|
                    Ok
                        (CCallExpr
                            { arguments = [ CLiteralExpr (CIntLiteral 0) ]
                            , caller = CPathExpr ( CIdentifierSegment "a", [] )
                            }
                        )
                , test "2 arguments"
                    "a(0, 1)"
                  <|
                    Ok
                        (CCallExpr
                            { arguments =
                                [ CLiteralExpr (CIntLiteral 0)
                                , CLiteralExpr (CIntLiteral 1)
                                ]
                            , caller = CPathExpr ( CIdentifierSegment "a", [] )
                            }
                        )
                , test "more complicated arguments"
                    "a(if true { 0 } else { 1 }, b(2, 3))"
                  <|
                    Ok
                        (CCallExpr
                            { arguments =
                                [ CIfExpr
                                    { condition = CLiteralExpr (CBoolLiteral True)
                                    , elseBody =
                                        CBlockExpr ( [], Just (CLiteralExpr (CIntLiteral 1)) )
                                    , thenBody = ( [], Just (CLiteralExpr (CIntLiteral 0)) )
                                    }
                                , CCallExpr
                                    { arguments = [ CLiteralExpr (CIntLiteral 2), CLiteralExpr (CIntLiteral 3) ]
                                    , caller = CPathExpr ( CIdentifierSegment "b", [] )
                                    }
                                ]
                            , caller = CPathExpr ( CIdentifierSegment "a", [] )
                            }
                        )
                ]
            ]
        ]


testDecl : Test
testDecl =
    let
        test : String -> String -> Result (List (DeadEnd Context Problem)) CleanDecl -> Test
        test description src expected =
            Test.test
                description
                (\_ -> Expect.equal expected (Result.map AntParser.cleanDecl <| AntParser.parseDecl src))
    in
    describe "Declarations"
        [ describe "struct declaration"
            [ test "simple"
                """struct MyStruct {
    a : String,
    b : Int,
}"""
              <|
                Ok
                    (CStructDecl
                        { fields = Dict.fromList [ ( "a", CStringType ), ( "b", CIntType ) ]
                        , name = "MyStruct"
                        }
                    )
            ]
        , describe "function declaration"
            [ test "0 arguments"
                "fn myFunc() : () {}"
              <|
                Ok
                    (CFnDecl
                        { body = ( [], Nothing )
                        , name = "myFunc"
                        , namespace = CModuleNamespace
                        , parameters = []
                        , returnType = CUnitType
                        }
                    )
            , test "2 arguments"
                """fn myFunc(a : Int, b : String) : String {
    var c = a * 2;
    b
}"""
              <|
                Ok
                    (CFnDecl
                        { body =
                            ( [ CVarStmt
                                    { target =
                                        CIdentifierPattern { mutable = False, name = "c" }
                                    , value =
                                        CArithmeticExpr
                                            { left = CPathExpr ( CIdentifierSegment "a", [] )
                                            , op = MultiplyOp
                                            , right = CLiteralExpr (CIntLiteral 2)
                                            }
                                    }
                              ]
                            , Just (CPathExpr ( CIdentifierSegment "b", [] ))
                            )
                        , name = "myFunc"
                        , namespace = CModuleNamespace
                        , parameters =
                            [ ( CIdentifierPattern { mutable = False, name = "a" }
                              , CIntType
                              )
                            , ( CIdentifierPattern { mutable = False, name = "b" }
                              , CStringType
                              )
                            ]
                        , returnType = CStringType
                        }
                    )
            ]
        , describe "impl decalration"
            [ test "one method"
                """impl MyStruct {
    fn getA(self: MyStruct) : Int {
        self.a
    }
}"""
              <|
                Ok
                    (CImplDecl
                        { functions =
                            Dict.fromList
                                [ ( "getA"
                                  , { body =
                                        ( []
                                        , Just
                                            (CPlaceExpr
                                                { accessor = CStructAccess "a"
                                                , target = CPathExpr ( CIdentifierSegment "self", [] )
                                                }
                                            )
                                        )
                                    , name = "getA"
                                    , namespace = CStructNamespace "MyStruct" Nothing
                                    , parameters =
                                        [ ( CIdentifierPattern { mutable = False, name = "self" }
                                          , CNamedType "MyStruct"
                                          )
                                        ]
                                    , returnType = CIntType
                                    }
                                  )
                                ]
                        , target = "MyStruct"
                        }
                    )
            , test "one function"
                """impl MyStruct {
    fn bar() : String {
        "MyStruct.bar() was called"
    }
}"""
              <|
                Ok
                    (CImplDecl
                        { functions =
                            Dict.fromList
                                [ ( "bar"
                                  , { body = ( [], Just (CLiteralExpr (CStringLiteral "MyStruct.bar() was called")) )
                                    , name = "bar"
                                    , namespace = CStructNamespace "MyStruct" Nothing
                                    , parameters = []
                                    , returnType = CStringType
                                    }
                                  )
                                ]
                        , target = "MyStruct"
                        }
                    )
            , test "1 method + 1 function"
                """impl MyStruct {
    fn bar() : String {
        "MyStruct.bar() was called"
    }
    fn getA(self: MyStruct) : Int {
        self.a
    }
}"""
              <|
                Ok
                    (CImplDecl
                        { functions =
                            Dict.fromList
                                [ ( "bar"
                                  , { body = ( [], Just (CLiteralExpr (CStringLiteral "MyStruct.bar() was called")) )
                                    , name = "bar"
                                    , parameters = []
                                    , returnType = CStringType
                                    , namespace = CStructNamespace "MyStruct" Nothing
                                    }
                                  )
                                , ( "getA"
                                  , { body =
                                        ( []
                                        , Just
                                            (CPlaceExpr
                                                { accessor = CStructAccess "a"
                                                , target = CPathExpr ( CIdentifierSegment "self", [] )
                                                }
                                            )
                                        )
                                    , name = "getA"
                                    , namespace = CStructNamespace "MyStruct" Nothing
                                    , parameters =
                                        [ ( CIdentifierPattern { mutable = False, name = "self" }
                                          , CNamedType "MyStruct"
                                          )
                                        ]
                                    , returnType = CIntType
                                    }
                                  )
                                ]
                        , target = "MyStruct"
                        }
                    )
            ]
        ]
