module ChordDiagram exposing (..)

{-| Drawing Chord Diagrams
-}

import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (width, height, viewBox, transform)
import SubPath
import ChordDiagram.LowLevel as LowLevel exposing (ChordGroup, Chord, drawBorder, drawChord)
import Color
import Color.Convert exposing (colorToCssRgb)


type alias Matrix =
    List ( String, List Float )


type alias Config =
    { groupConfigs : List GroupConfig
    , radius : Float
    , pulloutSize : Float
    , padding : Float
    }


type alias GroupConfig =
    { connectionStyle : { color : Color.Color, border : Border, opacity : Float }
    , nodeSegmentStyle : NodeSegmentStyle
    , label : Label
    }


type alias Border =
    { color : Color.Color, width : Float, opacity : Float }


type alias NodeSegmentStyle =
    { width : Float, color : Color.Color, border : Border, opacity : Float }


type alias Label =
    { text : String, orientation : Orientation, padding : Float, fontSize : String }


{-| A simle config for a ChordGroup

It takes a color as its argument and uses that
for the border, the chord border and the chord fill (with opacity 0.67)
-}
simpleConfig : Color.Color -> Config
simpleConfig primary =
    let
        border =
            { color = primary, width = 2, opacity = 1 }

        groupConfig =
            { connectionStyle =
                { color = primary, border = border, opacity = 0.67 }
            , nodeSegmentStyle =
                { width = 20, color = primary, border = border, opacity = 1 }
            , label = { text = "", orientation = orthogonal, padding = 20, fontSize = ".35em" }
            }
    in
        { groupConfigs = [ groupConfig ]
        , padding = degrees 5
        , radius = 200
        , pulloutSize = 0
        }


rotate : Float -> String
rotate angle =
    "rotate(" ++ toString angle ++ ")"


translate : Float -> Float -> String
translate x y =
    "translate(" ++ toString x ++ "," ++ toString y ++ ")"


type Orientation
    = Parallel
    | Orthogonal


parallel : Orientation
parallel =
    Parallel


orthogonal : Orientation
orthogonal =
    Orthogonal


rotation angle radius pullout orientation =
    case orientation of
        Orthogonal ->
            [ Attributes.textAnchor
                (if angle > 0.5 * pi && angle < 1.5 * pi then
                    "end"
                 else
                    "start"
                )
            , Attributes.transform <|
                String.concat
                    [ uncurry translate (fromPolar ( radius, angle ))
                    , translate pullout 0
                    , rotate (angle * 180 / pi)
                    , if angle > 0.5 * pi && angle < 1.5 * pi then
                        rotate 180
                      else
                        rotate 0
                    ]
            ]

        Parallel ->
            [ Attributes.textAnchor "middle"
            , Attributes.transform <|
                String.concat
                    [ uncurry translate (fromPolar ( radius, angle ))
                    , translate pullout 0
                    , rotate (angle * 180 / pi + 90)
                    ]
            ]


renderLabel : Float -> Float -> Float -> Label -> Svg msg
renderLabel angle radius pullout label =
    Svg.text_
        (rotation angle (radius + label.padding) pullout Orthogonal
            ++ [ Attributes.dy label.fontSize
               , Attributes.class "elm-chord-diagram-label"
               ]
        )
        [ Svg.text label.text ]


renderBorder : { a | startAngle : Float, endAngle : Float, width : Float, innerRadius : Float } -> NodeSegmentStyle -> Float -> Svg msg
renderBorder { startAngle, endAngle, width, innerRadius } segment pullout =
    LowLevel.drawBorder width innerRadius startAngle endAngle
        |> flip SubPath.element
            [ Attributes.stroke (colorToCssRgb segment.border.color)
            , Attributes.strokeWidth (toString segment.border.width)
            , Attributes.fill (colorToCssRgb segment.color)
            , Attributes.fillOpacity (toString segment.opacity)
            ]
        |> List.singleton
        |> Svg.g
            [ Attributes.transform <| translate pullout 0
            ]


pointsLeft angle =
    angle > 0.5 * pi && angle < 1.5 * pi


renderChord : { a | radius : Float, pulloutSize : Float } -> { b | border : Border, color : Color.Color, opacity : Float } -> Chord -> Svg msg
renderChord global config chord =
    let
        attributes =
            [ Attributes.stroke (colorToCssRgb config.border.color)
            , Attributes.strokeWidth (toString config.border.width)
            , Attributes.fill (colorToCssRgb config.color)
            , Attributes.fillOpacity (toString config.opacity)
            ]

        connects =
            SubPath.element
                (LowLevel.drawChord
                    { sourceRadius = global.radius
                    , targetRadius = global.radius
                    , pullout =
                        if ((chord.source.startAngle + chord.source.endAngle) / 2) < pi then
                            global.pulloutSize
                        else
                            -global.pulloutSize
                    }
                    chord
                )
                attributes
    in
        connects


renderChordGroup : { a | radius : Float, pulloutSize : Float, padding : Float } -> GroupConfig -> ChordGroup -> Svg msg
renderChordGroup global groupConfig chordGroup =
    let
        angle =
            ((chordGroup.startAngle + chordGroup.endAngle) / 2)

        pullout =
            if pointsLeft angle then
                -global.pulloutSize
            else
                global.pulloutSize

        label =
            renderLabel angle (global.radius + groupConfig.nodeSegmentStyle.width) pullout groupConfig.label

        border =
            let
                settings =
                    { startAngle = chordGroup.startAngle
                    , endAngle = chordGroup.endAngle
                    , width = groupConfig.nodeSegmentStyle.width
                    , innerRadius = global.radius
                    }
            in
                renderBorder settings groupConfig.nodeSegmentStyle pullout

        chords =
            List.map (renderChord global groupConfig.connectionStyle) chordGroup.connections
    in
        Svg.g [] (label :: border :: chords)


setText text groupConfig =
    let
        label =
            groupConfig.label
    in
        { groupConfig | label = { label | text = text } }


diagram : { width : Float, height : Float } -> Config -> Matrix -> Svg msg
diagram canvas config matrix =
    let
        ( labels, numbers ) =
            List.unzip matrix

        chords =
            LowLevel.createChords
                { padAngle = config.padding
                , startAngle = 0
                }
                numbers

        groupConfigs =
            let
                repeats =
                    ceiling (toFloat (List.length labels) / toFloat (List.length config.groupConfigs))
            in
                List.concat (List.repeat repeats config.groupConfigs)

        rendered =
            List.map3 (\label chordGroup groupConfig -> renderChordGroup config (setText label groupConfig) chordGroup) labels chords groupConfigs
    in
        Svg.g [ transform (translate (canvas.width / 2) (canvas.height / 2)) ] rendered
