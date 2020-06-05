module AntCheckerTest exposing (..)

import AntChecker exposing (Problem(..))
import AntParser exposing (Type(..))
import Expect
import Test exposing (Test, describe)


testDecls : Test
testDecls =
    let
        test : String -> String -> Result (List Problem) () -> Test
        test description src expected =
            Test.test
                description
                (\_ ->
                    case AntParser.parseDecls src of
                        Ok decls ->
                            Expect.equal expected (AntChecker.checkDecls decls)

                        Err parseErr ->
                            Expect.fail <| AntParser.showDeadEnds src parseErr
                )
    in
    describe "Test Decls"
        [ describe "StructDecl"
            [ test "normal StructDecl"
                """struct MyStruct {
    a : String,
    b : Int,
}"""
              <|
                Ok ()
            , test "DuplicatedStructDecl with same field names and types"
                """struct MyStruct {
    a : String,
    b : Int,
}
struct MyStruct {
    a : String,
    b : Int,
}"""
              <|
                Err
                    [ DuplicatedStructDecl
                        { from = ( 1, 8 ), to = ( 1, 16 ), value = "MyStruct" }
                        { from = ( 5, 8 ), to = ( 5, 16 ), value = "MyStruct" }
                    ]
            , test "DuplicatedStructDecl with same field names but different types"
                """struct MyStruct {
    a : String,
    b : Int,
}
struct MyStruct {
    a : String,
    b : String,
}"""
              <|
                Err
                    [ DuplicatedStructDecl
                        { from = ( 1, 8 ), to = ( 1, 16 ), value = "MyStruct" }
                        { from = ( 5, 8 ), to = ( 5, 16 ), value = "MyStruct" }
                    ]
            , test "DuplicatedStructDecl with different field names"
                """struct MyStruct {
    a : String,
    b : Int,
}
struct MyStruct {
    a : String,
    c : String,
}"""
              <|
                Err
                    [ DuplicatedStructDecl
                        { from = ( 1, 8 ), to = ( 1, 16 ), value = "MyStruct" }
                        { from = ( 5, 8 ), to = ( 5, 16 ), value = "MyStruct" }
                    ]
            , describe "FnDecl"
                [ test "normal FnDecl"
                    """fn foo(a : Int) : String {
    "abc"
}"""
                  <|
                    Ok ()
                , test "DuplicatedFunctionDecl"
                    """fn foo() : () {}
fn foo() : () {}"""
                  <|
                    Err
                        [ DuplicatedFunctionDecl
                            { from = ( 1, 4 ), to = ( 1, 7 ), value = "foo" }
                            { from = ( 2, 4 ), to = ( 2, 7 ), value = "foo" }
                        ]
                ]
            ]
        , describe "Variable"
            [ test "normal variable"
                """fn foo() : () {
    var mut a = 0;
    let a = 1;
}"""
              <|
                Ok ()
            , test "DuplicatedVariable"
                """fn foo() : () {
    var a = 0;
    var a = 1;
}"""
              <|
                Err
                    [ DuplicatedVariable
                        { from = ( 2, 9 ), to = ( 2, 10 ), value = "a" }
                        { from = ( 3, 9 ), to = ( 3, 10 ), value = "a" }
                    ]
            , test "UndefinedVariable"
                """fn foo() : () {
    let a = 0;
}"""
              <|
                Err [ UndefinedVariable { from = ( 2, 9 ), to = ( 2, 10 ), value = "a" } ]
            , test "MutateImmutableVariable"
                """fn foo() : () {
    var a = 0;
    let a = a + 1;
}"""
              <|
                Err [ MutateImmutableVariable { from = ( 3, 9 ), to = ( 3, 10 ), value = "a" } ]
            ]
        , describe "Types"
            [ test "UndefinedNamedType"
                "fn foo() : UndefinedStruct {}"
              <|
                Err [ UndefinedNamedType { from = ( 1, 12 ), to = ( 1, 27 ), value = "UndefinedStruct" } ]
            , test "normal structType"
                """struct MyStruct {
    a : Int
}
fn foo() : MyStruct {
    MyStruct {
        a = 0
    }
}"""
              <|
                Ok ()
            , test "UndefinedStructType"
                """fn foo() : () {
    UndefinedStruct {
        a = 0
    }
}"""
              <|
                Err [ UndefinedStructType { from = ( 2, 5 ), to = ( 2, 20 ), value = "UndefinedStruct" } ]
            , test "MismatchedTypes"
                "fn foo() : Int {}"
              <|
                Err
                    [ MismatchedTypes
                        { from = ( 1, 12 ), to = ( 1, 15 ), value = IntType }
                        { from = ( 1, 16 ), to = ( 1, 18 ), value = UnitType }
                    ]
            ]
        , describe "Namespace"
            [ test "UndefinedNamespace"
                "fn foo() : () { a::Bar }"
              <|
                Err [ UndefinedNamespace { from = ( 1, 17 ), to = ( 1, 18 ), value = "a" } ]
            ]
        , describe "Function"
            [ test "normal function"
                """fn foo(a : Int, b : Char) : Char {
    var c = a + 3;
    var d = c - 2;
    b
}
"""
              <|
                Ok ()
            , test "normal method"
                """struct MyStruct {
    a : Int
}
impl MyStruct {
    fn increment(mut self : MyStruct) : () {
        let self.a = self.a + 1;
    }
}
fn foo(s : MyStruct) : MyStruct {
    call MyStruct::increment(s);
    s
}
"""
              <|
                Ok ()
            , test "UndefinedFunction"
                """struct MyStruct {
    a : Int
}
impl MyStruct {
    fn increment(mut self : MyStruct) : () {
        let self.a = self.a + 1;
    }
}
fn foo(s : MyStruct) : MyStruct {
    call MyStruct::undefinedFunction(s);
    s
}
"""
              <|
                Err [ UndefinedFunction { from = ( 10, 20 ), to = ( 10, 37 ), value = "undefinedFunction" } ]
            ]
        ]
