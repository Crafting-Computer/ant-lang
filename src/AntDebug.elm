module AntDebug exposing (main)

import AntParser
import AntChecker
import Html exposing (div, pre, text)


source =
    """
struct MyStruct {
    a : String,
    b : MyStruct2,
}
struct MyStruct2 {
    a : String,
    b : Int,
}
fn createMyStruct(id : Int) : MyStruct {
    MyStruct {
        a = "abc",
        b = if id > 0 {
            MyStruct2 { a = "abc", b = 0 }
        } else {
            MyStruct2 { a = "abc", b = 1 }
        }
    }
}
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
