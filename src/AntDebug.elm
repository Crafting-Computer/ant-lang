module AntDebug exposing (main)

import AntParser
import Html exposing (div, pre, text)


source =
    """
struct MyStruct {
    a : String,
    b : Int,
}
struct MyStruct2 {
    a : String,
    b : Int,
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

        Ok _ ->
            div []
                [ pre [] [ text "✔️ Passed parser." ]
                ]
