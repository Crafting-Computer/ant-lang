module AntDebug exposing (main)

import AntParser
import AntChecker
import Html exposing (div, pre, text)


source =
--     """
-- struct MyStruct {
--     a : String,
--     b : MyStruct2,
-- }
-- struct MyStruct2 {
--     a : String,
--     b : Int,
-- }
-- fn createMyStruct(id : Int) : MyStruct {
--     MyStruct {
--         a = "abc",
--         b = if id > 0 {
--             MyStruct2 { a = "abc", b = 0 }
--         } else {
--             MyStruct2 { a = "abc", b = 1 }
--         }
--     }
-- }
-- """
--     """struct MyStruct {
--     a : Int
-- }
-- impl MyStruct {
--     fn increment(mut self : MyStruct) : () {
--         let self.a = self.a + 1;
--     }
-- }
-- fn foo(s : MyStruct) : MyStruct {
--     call MyStruct::increment(s);
--     s
-- }
-- """
--     """fn foo(a : Int, _ : Char) : Int {
--     var _ = a + 3;
--     var d = a - 2;
--     a
-- }
-- """
--     """
-- struct Person {
--     name : String,
--     age : Int
-- }

-- trait Show {
--     fn show(self : Self) : String;
--     fn changeName(self : Self, newName : String) : ();
-- }

-- impl Show for Person {
--     fn show(self : Self) : String {
--         "wala"
--     }
--     fn changeName(mut self: Self, newName : String) : () {
--         let self.name = newName;
--         call <Person as Show>::show(self);
--     }
-- }

--     """
    """
fn identity(a : Int) : Int { a }
fn add(a : Int, b : Int) : Int { identity(a) + identity(a, b) }
    """


main =
    case AntParser.parseDecls source of
        Err err ->
            div []
                [ pre []
                    [ text <|
                        "❌ Parse error.\n\n"
                            ++ AntParser.showDeadEnds source err
                    ]
                ]

        Ok decls ->
            div []
                [ pre [] [ text "✔️ Passed parser." ]
                , pre [] [ text <| 
                    case AntChecker.checkDecls decls of
                        Ok _ ->
                            "✔️ Passed checker."
                        
                        Err problems ->
                            AntChecker.showProblems source problems
                ]
                ]
