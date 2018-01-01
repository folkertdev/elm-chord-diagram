module Stretched exposing (..)

import ChordDiagram exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (width, height, viewBox, transform)
import SubPath
import Path.LowLevel
import LowLevel.Command as Command exposing (smallestArc, largestArc, clockwise, counterClockwise)
import List.Extra as List
import Vector2 as Vec2 exposing (Vec2)
import Matrix2 exposing (Mat2)
import Segment
import Color
import Color.Convert exposing (colorToCssRgb)
import List.Extra


respondents =
    95


emptyPerc =
    0.4


emptyStroke =
    (respondents * emptyPerc)


offset =
    ((2 * pi) * (emptyStroke / (respondents + emptyStroke)) / 4)


(=>) =
    (,)


matrix =
    matrix2


matrix1 =
    [ "A" => [ 0, 1, 1 ]
    , "B" => [ 1, 0, 1 ]
    , "Ð" => [ 1, 1, 0 ]
    ]


matrix2 =
    [ "A" => [ 0, 0, 0, 0, 10, 5, 15, 0 ]
    , "B" => [ 0, 0, 0, 0, 5, 15, 20, 0 ]
    , "C" => [ 0, 0, 0, 0, 15, 5, 5, 0 ]
    , "Dummy1" => [ 0, 0, 0, 0, 0, 0, 0, emptyStroke ]
    , "X" => [ 5, 15, 5, 0, 0, 0, 0, 0 ]
    , "Y" => [ 15, 20, 5, 0, 0, 0, 0, 0 ]
    , "Z" => [ 10, 5, 15, 0, 0, 0, 0, 0 ]
    , "Dummy2" => [ 0, 0, 0, emptyStroke, 0, 0, 0, 0 ]
    ]


palette =
    { grey = Color.rgb 196 196 196
    , blue = Color.rgb 0 161 222
    }


defaultConfig : Config
defaultConfig =
    let
        border =
            { color = palette.grey, width = 0, opacity = 0 }

        connector =
            { color = palette.grey, border = border, opacity = 0.67 }

        source =
            { width = 20, color = palette.blue, border = border, opacity = 1 }
    in
        { connector = connector
        , source = source
        , label = "foo"
        , radius = 300
        , offset = offset
        , pulloutSize = 0
        }


invisibleBorder =
    { color = palette.grey, width = 0, opacity = 0 }


invisibleConnector =
    { color = Color.red, border = invisibleBorder, opacity = 0.0 }


invisibleSource =
    { width = 20, color = palette.blue, border = invisibleBorder, opacity = 0 }


main =
    let
        ( labels_, numbers ) =
            List.unzip matrix

        labels =
            labels_

        gap =
            degrees 20

        groups =
            numbers
                |> chord { padAngle = gap, startAngle = -offset + gap / 2 }

        categories =
            List.indexedMap mapper groups
    in
        Svg.svg [ width "1200", height "1200" ]
            [ Svg.g [ transform "translate(550,550)" ] <|
                List.indexedMap
                    (\i ( label, category ) ->
                        if i == 3 || i == 7 then
                            renderCategory { defaultConfig | label = "", source = invisibleSource } { category | connections = [] }
                        else
                            renderCategory { defaultConfig | label = label } category
                    )
                    (List.map2 (,) labels categories)
            ]


mapper i { startAngle, endAngle, connections, value } =
    { startAngle = startAngle
    , endAngle = endAngle
    , connections = connections
    , value = value
    }
