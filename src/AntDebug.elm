module AntDebug exposing (main)

import AntParser
import Html exposing (div, pre, text)


source =
    """
var a =
  if true {
    1
  } else {
    0
  };

let a = 3;

var b =
  if true {
    2
  } else if false {
    1
  } else {
    0
  };
  """


main =
    case AntParser.parseStmts source of
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