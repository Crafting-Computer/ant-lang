module AntParserTest exposing (..)

import AntParser
    exposing
        ( Accessor(..)
        , ArithmeticOp(..)
        , CleanExpr(..)
        , CleanStmt(..)
        , ComparisonOp(..)
        , Context
        , Expr(..)
        , Literal(..)
        , Pattern(..)
        , Problem(..)
        , Stmt(..)
        , cleanExpr
        )
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Location exposing (Located, dummyLocated)
import Parser.Advanced exposing (DeadEnd)
import Test exposing (..)


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
            , testWithLocation "String"
                "\"it's a nice day!\""
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 19 ), value = StringLiteral "it's a nice day!" }
            , testWithLocation "Int"
                "38391900"
              <|
                Ok <|
                    LiteralExpr <|
                        { from = ( 1, 1 ), to = ( 1, 9 ), value = IntLiteral 38391900 }
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
                Ok (LiteralExpr { from = ( 1, 1 ), to = ( 3, 2 ), value = StructLiteral { mappings = Dict.fromList [ ( "key1", ( { from = ( 2, 3 ), to = ( 2, 7 ), value = "key1" }, { from = ( 2, 10 ), to = ( 2, 18 ), value = LiteralExpr { from = ( 2, 10 ), to = ( 2, 18 ), value = StringLiteral "value1" } } ) ) ], name = { from = ( 1, 1 ), to = ( 1, 9 ), value = "MyStruct" } } })
            ]
        , describe "ArithmeticExpr"
            [ test "add"
                "2 + 3 + 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = AddOp
                                , right = CLiteralExpr (IntLiteral 3)
                                }
                        , op = AddOp
                        , right = CLiteralExpr (IntLiteral 4)
                        }
                    )
            , test "subtract"
                "2 - 3 - 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = SubtractOp
                                , right = CLiteralExpr (IntLiteral 3)
                                }
                        , op = SubtractOp
                        , right = CLiteralExpr (IntLiteral 4)
                        }
                    )
            , test "mixed add and subtract"
                "2 + 3 - 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = AddOp
                                , right = CLiteralExpr (IntLiteral 3)
                                }
                        , op = SubtractOp
                        , right = CLiteralExpr (IntLiteral 4)
                        }
                    )
            , test "multiply"
                "2 * 3 * 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = MultiplyOp
                                , right = CLiteralExpr (IntLiteral 3)
                                }
                        , op = MultiplyOp
                        , right = CLiteralExpr (IntLiteral 4)
                        }
                    )
            , test "divide"
                "2 / 3 / 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = DivideOp
                                , right = CLiteralExpr (IntLiteral 3)
                                }
                        , op = DivideOp
                        , right = CLiteralExpr (IntLiteral 4)
                        }
                    )
            , test "mixed multiply and divide"
                "2 * 3 / 4"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = MultiplyOp
                                , right = CLiteralExpr (IntLiteral 3)
                                }
                        , op = DivideOp
                        , right = CLiteralExpr (IntLiteral 4)
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
                                    CLiteralExpr (IntLiteral 1)
                                , op = AddOp
                                , right =
                                    CArithmeticExpr
                                        { left = CLiteralExpr (IntLiteral 2)
                                        , op = MultiplyOp
                                        , right = CLiteralExpr (IntLiteral 3)
                                        }
                                }
                        , op =
                            SubtractOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 4)
                                , op = DivideOp
                                , right = CLiteralExpr (IntLiteral 5)
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
                                { left = CLiteralExpr (IntLiteral 1)
                                , op = BitwiseAndOp
                                , right = CLiteralExpr (IntLiteral 2)
                                }
                        , op = BitwiseAndOp
                        , right = CLiteralExpr (IntLiteral 3)
                        }
                    )
            , test "Bitwise OR"
                "1 | 2 | 3"
              <|
                Ok
                    (CArithmeticExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 1)
                                , op = BitwiseOrOp
                                , right = CLiteralExpr (IntLiteral 2)
                                }
                        , op = BitwiseOrOp
                        , right = CLiteralExpr (IntLiteral 3)
                        }
                    )
            , test "mixed Bitwise AND and OR"
                "1 | 2 & 3"
              <|
                Ok
                    (CArithmeticExpr
                        { left = CLiteralExpr (IntLiteral 1)
                        , op = BitwiseOrOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 2)
                                , op = BitwiseAndOp
                                , right = CLiteralExpr (IntLiteral 3)
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
                                { left = CLiteralExpr (IntLiteral 1)
                                , op = BitwiseAndOp
                                , right =
                                    CArithmeticExpr
                                        { left = CLiteralExpr (IntLiteral 5)
                                        , op = AddOp
                                        , right = CLiteralExpr (IntLiteral 2)
                                        }
                                }
                        , op = BitwiseOrOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 3)
                                , op = DivideOp
                                , right = CLiteralExpr (IntLiteral 4)
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
                        { left = CLiteralExpr (IntLiteral 0)
                        , op = EqualOp
                        , right = CLiteralExpr (IntLiteral 1)
                        }
                    )
            , test "not equal"
                "0 != 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (IntLiteral 0)
                        , op = NotEqualOp
                        , right = CLiteralExpr (IntLiteral 1)
                        }
                    )
            , test "greater than or equal"
                "0 >= 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (IntLiteral 0)
                        , op = GreaterThanOrEqualOp
                        , right = CLiteralExpr (IntLiteral 1)
                        }
                    )
            , test "less than or equal"
                "0 <= 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (IntLiteral 0)
                        , op = LessThanOrEqualOp
                        , right = CLiteralExpr (IntLiteral 1)
                        }
                    )
            , test "greater than"
                "0 > 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (IntLiteral 0)
                        , op = GreaterThanOp
                        , right = CLiteralExpr (IntLiteral 1)
                        }
                    )
            , test "less than"
                "0 < 1"
              <|
                Ok
                    (CComparisonExpr
                        { left = CLiteralExpr (IntLiteral 0)
                        , op = LessThanOp
                        , right = CLiteralExpr (IntLiteral 1)
                        }
                    )
            , test "mixed arithmetic and comparison operations"
                "1 + 2 < 3 * 4"
              <|
                Ok
                    (CComparisonExpr
                        { left =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 1)
                                , op = AddOp
                                , right = CLiteralExpr (IntLiteral 2)
                                }
                        , op = LessThanOp
                        , right =
                            CArithmeticExpr
                                { left = CLiteralExpr (IntLiteral 3)
                                , op = MultiplyOp
                                , right = CLiteralExpr (IntLiteral 4)
                                }
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
                            { from = ( 3, 8 ), to = ( 5, 2 ), value = BlockExpr { from = ( 3, 8 ), to = ( 5, 2 ), value = ( [], Just <| { from = ( 4, 3 ), to = ( 4, 4 ), value = LiteralExpr { from = ( 4, 3 ), to = ( 4, 4 ), value = IntLiteral 0 } } ) } }
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
                            CLiteralExpr (BoolLiteral True)
                        , elseBody =
                            CIfExpr
                                { condition =
                                    CLiteralExpr (BoolLiteral False)
                                , elseBody = CBlockExpr ( [], Just (CLiteralExpr (IntLiteral 0)) )
                                , thenBody = ( [], Just (CLiteralExpr (IntLiteral 1)) )
                                }
                        , thenBody = ( [], Just (CLiteralExpr (IntLiteral 2)) )
                        }
                    )
            ]
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
                        CLiteralExpr (BoolLiteral True)
                    , elseBody =
                        CIfExpr
                            { condition =
                                CLiteralExpr (BoolLiteral False)
                            , elseBody =
                                CIfExpr
                                    { condition =
                                        CLiteralExpr (BoolLiteral True)
                                    , elseBody =
                                        CBlockExpr ( [], Just (CLiteralExpr (IntLiteral 0)) )
                                    , thenBody =
                                        ( [], Just (CLiteralExpr (IntLiteral 1)) )
                                    }
                            , thenBody = ( [], Just (CLiteralExpr (IntLiteral 2)) )
                            }
                    , thenBody = ( [], Just (CLiteralExpr (IntLiteral 3)) )
                    }
                )
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
                                        IdentifierPattern
                                            { mutable = False, name = { from = ( 2, 9 ), to = ( 2, 10 ), value = "a" } }
                                    , value = CLiteralExpr (IntLiteral 0)
                                    }
                              , CLetStmt
                                    { target =
                                        CPlaceExpr { accessors = [], name = "a" }
                                    , value = CLiteralExpr (IntLiteral 1)
                                    }
                              ]
                            , Nothing
                            )
                        , condition = CLiteralExpr (BoolLiteral True)
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
                                        IdentifierPattern
                                            { mutable = False, name = { from = ( 2, 9 ), to = ( 2, 10 ), value = "a" } }
                                    , value = CLiteralExpr (IntLiteral 0)
                                    }
                              , CLetStmt
                                    { target =
                                        CPlaceExpr { accessors = [], name = "a" }
                                    , value = CLiteralExpr (IntLiteral 1)
                                    }
                              ]
                            , Just <|
                                CPlaceExpr
                                    { name =
                                        "a"
                                    , accessors =
                                        []
                                    }
                            )
                        , condition = CLiteralExpr (BoolLiteral True)
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
                            , Just (CLiteralExpr (IntLiteral 1))
                            )
                        , condition = CLiteralExpr (BoolLiteral True)
                        }
                    )
            ]
        , describe "PlaceExpr"
            [ test "array access"
                "a[0]"
              <|
                Ok
                    (CPlaceExpr
                        { name = "a"
                        , accessors =
                            [ ArrayAccess
                                { from = ( 1, 3 )
                                , to = ( 1, 4 )
                                , value = LiteralExpr { from = ( 1, 3 ), to = ( 1, 4 ), value = IntLiteral 0 }
                                }
                            ]
                        }
                    )
            , test "struct access"
                "a.fieldName"
              <|
                Ok
                    (CPlaceExpr
                        { accessors =
                            [ StructAccess { from = ( 1, 3 ), to = ( 1, 12 ), value = "fieldName" } ]
                        , name = "a"
                        }
                    )
            , test "string of struct accesses"
                "a.fieldName1.fieldName2"
              <|
                Ok
                    (CPlaceExpr
                        { accessors =
                            [ StructAccess { from = ( 1, 3 ), to = ( 1, 13 ), value = "fieldName1" }
                            , StructAccess { from = ( 1, 14 ), to = ( 1, 24 ), value = "fieldName2" }
                            ]
                        , name = "a"
                        }
                    )
            , test "mixed accesses"
                "a[0].fieldName1.fieldName2[1][2]"
              <|
                Ok
                    (CPlaceExpr
                        { accessors =
                            [ ArrayAccess
                                { from = ( 1, 3 )
                                , to = ( 1, 4 )
                                , value = LiteralExpr { from = ( 1, 3 ), to = ( 1, 4 ), value = IntLiteral 0 }
                                }
                            , StructAccess { from = ( 1, 6 ), to = ( 1, 16 ), value = "fieldName1" }
                            , StructAccess { from = ( 1, 17 ), to = ( 1, 27 ), value = "fieldName2" }
                            , ArrayAccess
                                { from = ( 1, 28 )
                                , to = ( 1, 29 )
                                , value =
                                    LiteralExpr { from = ( 1, 28 ), to = ( 1, 29 ), value = IntLiteral 1 }
                                }
                            , ArrayAccess
                                { from = ( 1, 31 )
                                , to = ( 1, 32 )
                                , value =
                                    LiteralExpr { from = ( 1, 31 ), to = ( 1, 32 ), value = IntLiteral 2 }
                                }
                            ]
                        , name = "a"
                        }
                    )
            , describe "CallExpr"
                [ test "no argument"
                    "a()"
                  <|
                    Ok (CCallExpr { arguments = [], caller = CPlaceExpr { accessors = [], name = "a" } })
                , test "1 argument"
                    "a(0)"
                  <|
                    Ok
                        (CCallExpr
                            { arguments = [ CLiteralExpr (IntLiteral 0) ]
                            , caller = CPlaceExpr { accessors = [], name = "a" }
                            }
                        )
                , test "2 arguments"
                    "a(0, 1)"
                  <|
                    Ok
                        (CCallExpr
                            { arguments =
                                [ CLiteralExpr (IntLiteral 0)
                                , CLiteralExpr (IntLiteral 1)
                                ]
                            , caller = CPlaceExpr { accessors = [], name = "a" }
                            }
                        )
                , test "more complicated arguments"
                    "a(if true { 0 } else { 1 }, b(2, 3))"
                  <|
                    Ok
                        (CCallExpr
                            { arguments =
                                [ CIfExpr
                                    { condition = CLiteralExpr (BoolLiteral True)
                                    , elseBody =
                                        CBlockExpr ( [], Just (CLiteralExpr (IntLiteral 1)) )
                                    , thenBody = ( [], Just (CLiteralExpr (IntLiteral 0)) )
                                    }
                                , CCallExpr
                                    { arguments = [ CLiteralExpr (IntLiteral 2), CLiteralExpr (IntLiteral 3) ]
                                    , caller = CPlaceExpr { accessors = [], name = "b" }
                                    }
                                ]
                            , caller = CPlaceExpr { accessors = [], name = "a" }
                            }
                        )
                ]
            ]
        ]



-- testStmts : Test
-- testStmts =
--   let
--     test : String -> String -> Result (List (DeadEnd Context Problem)) (List Stmt) -> Test
--     test description src expected =
--       Test.test
--       description
--       (\_ -> Expect.equal expected (AntParser.parseStmts src))
--   in
