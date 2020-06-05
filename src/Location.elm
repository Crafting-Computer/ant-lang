module Location exposing (..)

import List.Extra


showProblemLocation : Int -> Int -> String -> String
showProblemLocation row col src =
    let
        rawLine =
            getLine row src

        lineNumber =
            row

        line =
            String.fromInt lineNumber ++ "| " ++ (String.trimLeft <| rawLine)

        offset =
            String.length line - String.length rawLine - 1

        offsettedCol =
            offset + col

        underline =
            makeUnderline line offsettedCol (offsettedCol + 1)
    in
    line ++ "\n" ++ underline


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
    String.toList (row ++ " ")
        |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
        |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
    if minCol <= col && col < maxCol then
        '^'

    else
        ' '


getLine : Int -> String -> String
getLine row src =
    Maybe.withDefault ("CAN'T GET LINE AT ROW " ++ String.fromInt row)
    -- impossible
    <|
        List.Extra.getAt (row - 1) <|
            String.split "\n" src


showLocation : String -> Located a -> String
showLocation src location =
    let
        ( fromRow, fromCol ) =
            location.from

        ( toRow, toCol ) =
            location.to
    in
    showLocationRange fromRow fromCol toRow toCol src


showLocationRange : Int -> Int -> Int -> Int -> String -> String
showLocationRange startRow startCol endRow endCol src =
    String.join "\n" <|
        List.map
            (\row ->
                let
                    rawLine =
                        getLine row src

                    line =
                        String.fromInt row ++ "| " ++ (String.trimLeft <| rawLine)

                    offset =
                        String.length line - String.length rawLine - 1

                    underlineStartCol =
                        if row == startRow then
                            offset + startCol

                        else
                            1

                    underlineEndCol =
                        if row == endRow then
                            offset + endCol

                        else
                            String.length line

                    underline =
                        makeUnderline line underlineStartCol underlineEndCol
                in
                line ++ "\n" ++ underline
            )
            (List.range startRow endRow)


type alias Located a =
    { from : ( Int, Int )
    , value : a
    , to : ( Int, Int )
    }


dummyLocated : a -> Located a
dummyLocated a =
    { from =
        ( -1, -1 )
    , to =
        ( -1, -1 )
    , value =
        a
    }

withLocation : Located a -> b -> Located b
withLocation loc value =
  { from =
    loc.from
  , to =
    loc.to
  , value =
    value
  }


changeLocation : Located a -> Located b -> Located b
changeLocation loc value =
  withLocation loc value.value


getLocationFromList : List (Located a) -> Located ()
getLocationFromList locations =
    case locations of
        [] -> -- impossible
            dummyLocated ()
        
        firstLocation :: restLocations ->
            { from =
                firstLocation.from
            , to =
                case List.Extra.last restLocations of
                    Just lastLocation ->
                        lastLocation.to
                    
                    Nothing ->
                        firstLocation.to
            , value =
                ()
            }