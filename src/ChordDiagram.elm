module ChordDiagram exposing (..)

{-| Drawing Chord Diagrams
-}

import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (width, height, viewBox, transform)
import SubPath
import LowLevel.Command as Command exposing (smallestArc, largestArc, clockwise, counterClockwise)
import List.Extra as List
import Color
import Color.Convert exposing (colorToCssRgb)
import Arc


type alias Border =
    { color : Color.Color, width : Float, opacity : Float }


type alias Config =
    { connector : { color : Color.Color, border : Border, opacity : Float }
    , source : { width : Float, color : Color.Color, border : Border, opacity : Float }
    , label : String
    , radius : Float
    , offset : Float
    , pulloutSize : Float
    }


defaultConfig : Color.Color -> Config
defaultConfig primary =
    let
        border =
            { color = primary, width = 2, opacity = 1 }

        connector =
            { color = primary, border = border, opacity = 0.67 }

        source =
            { width = 20, color = primary, border = border, opacity = 1 }
    in
        { connector = connector
        , source = source
        , label = "foo"
        , radius = 200
        , offset = pi
        , pulloutSize = 0
        }


rotate angle =
    "rotate(" ++ toString angle ++ ")"


translate x y =
    "translate(" ++ toString x ++ "," ++ toString y ++ ")"


renderCategory : Config -> Category -> Svg msg
renderCategory config category =
    let
        angle =
            ((category.startAngle + category.endAngle) / 2)

        pullout =
            if angle > 0.5 * pi && angle < 1.5 * pi then
                -config.pulloutSize
            else
                config.pulloutSize

        label =
            Svg.text_
                [ Attributes.dy ".35em"
                , Attributes.class "titles"
                , Attributes.textAnchor
                    (if angle > pi then
                        "end"
                     else
                        "start"
                    )
                , Attributes.transform <|
                    String.concat
                        [ uncurry translate (fromPolar ( config.radius + config.source.width + 55, angle ))
                        , translate pullout 0
                        , rotate (angle * 180 / pi)
                        , if angle > 0.5 * pi && angle < 1.5 * pi then
                            rotate 180
                          else
                            rotate 0
                        ]
                ]
                [ Svg.text config.label ]

        b =
            border config.source.width config.radius category.startAngle category.endAngle
                |> flip SubPath.element
                    [ Attributes.stroke (colorToCssRgb config.source.border.color)
                    , Attributes.strokeWidth (toString config.connector.border.width)
                    , Attributes.fill (colorToCssRgb config.source.color)
                    , Attributes.fillOpacity (toString config.source.opacity)
                    ]
                |> List.singleton
                |> Svg.g
                    [ Attributes.transform <| translate pullout 0
                    ]

        attributes =
            [ Attributes.stroke (colorToCssRgb config.connector.border.color)
            , Attributes.strokeWidth (toString config.connector.border.width)
            , Attributes.fill (colorToCssRgb config.connector.color)
            , Attributes.fillOpacity (toString config.connector.opacity)
            ]

        connects =
            List.map
                (\current ->
                    SubPath.element
                        (drawRibbon
                            { sourceRadius = config.radius
                            , targetRadius = config.radius
                            , pullout =
                                if ((current.source.startAngle + current.source.endAngle) / 2) < pi then
                                    config.pulloutSize
                                else
                                    -config.pulloutSize
                            }
                            current
                        )
                        attributes
                )
                category.connections
    in
        Svg.g [] (label :: b :: connects)



{-
   main =
       let
           colors =
               [ "#000000", "#FFDD89", "#957244", "#F26223" ]
                   |> List.map (Color.Convert.hexToColor >> Result.withDefault Color.black)

           makeAttributes : String -> List (Svg.Attribute msg)
           makeAttributes color =
               [ Attributes.fillOpacity "0.67", Attributes.stroke color, Attributes.fill color ]

           groups =
               chord { padAngle = degrees 5, startAngle = 0 } matrix

           config =
               defaultConfig

           categories : List Category
           categories =
               groups

           -- |> List.indexedMap (\i e -> List.take (1 + i) e)
       in
           Svg.svg [ width "1200", height "1200" ]
               [ Svg.g [ transform "translate(550,550)" ] <| List.map2 (\color category -> renderCategory (defaultConfig color) category) (colors ++ colors) categories
               ]
-}
