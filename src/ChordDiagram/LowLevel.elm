module ChordDiagram.LowLevel exposing (ChordGroup, SubGroup, Chord, createChords, drawBorder, drawChord)

{-| Drawing Chord Diagrams
-}

import SubPath
import LowLevel.Command as Command exposing (smallestArc, largestArc, clockwise, counterClockwise)
import List.Extra as List
import Arc


-- Phase 1 - partition the circle


{-| -}
type alias Group =
    { startAngle : Float, endAngle : Float, value : Float, connections : List SubGroup }


type alias SubGroup =
    { startAngle : Float, endAngle : Float, value : Float }


{-| -}
divideCircle : { padAngle : Float, startAngle : Float } -> List (List Float) -> List Group
divideCircle { padAngle, startAngle } rows =
    let
        -- calculate the size of one unit value
        tau =
            2 * pi

        n : Int
        n =
            List.length rows

        totalSum =
            List.map List.sum rows
                |> List.sum

        radiansPerUnit =
            -- radians per unit "value"
            max 0 ((tau - padAngle * toFloat n) / totalSum)

        paddingDistance =
            if radiansPerUnit /= 0 && not (isNaN radiansPerUnit) then
                padAngle
            else
                tau / toFloat n

        ( _, _, groups ) =
            -- for every row, give it (and its columns) their alloted space
            partitionGroup radiansPerUnit paddingDistance startAngle rows
    in
        groups


atom : Float -> Float -> Float -> ( Float, SubGroup )
atom k startAngle value =
    let
        endAngle =
            value * k + startAngle
    in
        ( endAngle
        , { startAngle = startAngle, endAngle = endAngle, value = value }
        )


partitionSubgroup : Float -> Float -> List Float -> ( Float, Float, List SubGroup )
partitionSubgroup k startAngle values =
    let
        folder v ( angle, accumValue, accum ) =
            let
                ( newAngle, datum ) =
                    atom k angle v
            in
                ( newAngle, accumValue + v, datum :: accum )
    in
        List.foldl folder ( startAngle, 0, [] ) values


partitionGroup : Float -> Float -> Float -> List (List Float) -> ( Float, Float, List Group )
partitionGroup k dx startAngle values =
    let
        folder v ( angle, accumValue, accum ) =
            let
                ( newAngle, added, datum ) =
                    partitionSubgroup k angle v

                new =
                    { startAngle = angle
                    , endAngle = newAngle
                    , connections = datum
                    , value = added
                    }
            in
                ( newAngle + dx
                , accumValue + added
                , new :: accum
                )
    in
        List.foldr folder ( startAngle, 0, [] ) values



-- Phase 2, make the connections between subgroups


type alias Chord =
    { source : SubGroup
    , target : SubGroup
    }


type alias ChordGroup =
    { startAngle : Float
    , endAngle : Float
    , value : Float
    , connections : List Chord
    }


createChords : { padAngle : Float, startAngle : Float } -> List (List Float) -> List ChordGroup
createChords config rows =
    let
        groups =
            divideCircle config rows

        mapper { startAngle, endAngle, value } relation =
            { startAngle = startAngle - pi / 2
            , endAngle = endAngle - pi / 2
            , value = value
            , connections = relation
            }
    in
        List.map2 mapper groups (connections groups)


connections : List Group -> List (List Chord)
connections groups =
    let
        rows =
            List.map .connections groups
                |> List.reverse

        columns =
            List.transpose rows

        mapper source target =
            if source.value == 0 && target.value == 0 then
                Nothing
            else if source.value == target.value then
                -- we have to make a choice here, angles are unique
                if source.startAngle < target.startAngle then
                    Nothing
                else
                    Just { source = source, target = target }
            else if source.value > target.value then
                Nothing
            else
                Just { source = source, target = target }
    in
        List.map2 (\row column -> List.map2 mapper row column) rows columns
            |> List.map (List.filterMap identity)
            |> List.reverse



-- Phase 3 - convert to paths


drawChord : { sourceRadius : Float, targetRadius : Float, pullout : Float } -> Chord -> SubPath.SubPath
drawChord { sourceRadius, targetRadius, pullout } { source, target } =
    if source.value == 0 && target.value == 0 then
        SubPath.empty
    else
        let
            halfPi =
                pi / 2

            ( center1, center2 ) =
                -- centers for the "source" arcs, respecting pullout
                ( ( pullout, 0 )
                , ( -pullout, 0 )
                )

            ( sa0, sa1 ) =
                ( source.startAngle - halfPi
                , source.endAngle - halfPi
                )

            ( ta0, ta1 ) =
                ( target.startAngle - halfPi
                , target.endAngle - halfPi
                )

            s0 =
                fromPolar ( sourceRadius, sa0 )
                    |> Tuple.mapFirst (\x -> x + pullout)

            t0 =
                fromPolar ( targetRadius, ta0 )
                    |> Tuple.mapFirst (\x -> x - pullout)

            quadratic =
                Command.quadraticCurveTo << List.singleton
        in
            if sa0 == ta0 && sa1 == ta1 then
                -- source and target are the same
                SubPath.subpath (Command.MoveTo s0) <|
                    [ arc center1 sourceRadius sa0 sa1 clockwise
                    , quadratic ( ( 0, 0 ), s0 )
                    , Command.closePath
                    ]
            else
                SubPath.subpath (Command.MoveTo s0) <|
                    [ arc center1 sourceRadius sa0 sa1 clockwise
                    , quadratic ( ( 0, 0 ), t0 )
                    , arc center2 targetRadius ta0 ta1 clockwise
                    , quadratic ( ( 0, 0 ), s0 )
                    , Command.closePath
                    ]


arc : ( Float, Float ) -> Float -> Float -> Float -> Command.Direction -> Command.DrawTo
arc center r a0 a1 direction =
    let
        da =
            if direction == counterClockwise then
                a0 - a1
            else
                a1 - a0
    in
        { radii = ( r, r )
        , center = center
        , xAxisRotate = 0
        , startAngle = a0
        , deltaTheta = da
        }
            |> Arc.arc


drawBorder : Float -> Float -> Float -> Float -> SubPath.SubPath
drawBorder width innerRadius endAngle startAngle =
    let
        outerRadius =
            innerRadius + width

        firstStart =
            fromPolar ( innerRadius, startAngle )

        secondStart =
            fromPolar ( innerRadius + width, endAngle )

        firstTarget =
            fromPolar ( innerRadius, endAngle )

        secondTarget =
            fromPolar ( outerRadius, startAngle )

        firstConfig =
            { center = ( 0, 0 )
            , radii = ( innerRadius, innerRadius )
            , startAngle = startAngle
            , deltaTheta = endAngle - startAngle
            , xAxisRotate = 0
            }

        secondConfig =
            { center = ( 0, 0 )
            , radii = ( outerRadius, outerRadius )
            , startAngle = endAngle
            , deltaTheta = startAngle - endAngle
            , xAxisRotate = 0
            }
    in
        SubPath.subpath (Command.moveTo firstStart)
            [ Arc.arc firstConfig
            , Command.lineTo [ secondStart ]
            , Arc.arc secondConfig
            , Command.closePath
            ]
