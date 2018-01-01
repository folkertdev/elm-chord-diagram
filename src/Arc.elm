module Arc exposing (arc, CenterParameterization)

import Path.LowLevel
import Vector2 as Vec2 exposing (Vec2)
import Matrix2 exposing (Mat2)
import LowLevel.Command as Command exposing (smallestArc, largestArc, clockwise, counterClockwise)
import Segment
import SubPath


arc : CenterParameterization -> Command.DrawTo
arc config =
    let
        subpath =
            Segment.Arc (centerToEndpoint config)
                |> List.singleton
                |> SubPath.fromSegments
    in
        case SubPath.unwrap subpath of
            Nothing ->
                Debug.crash "`arc` encountered an empty subpath"

            Just { drawtos } ->
                case drawtos of
                    [ arcCommand ] ->
                        arcCommand

                    _ ->
                        Debug.crash ("`arc` expected just one command, but got: " ++ toString drawtos)


type alias CenterParameterization =
    { center : Vec2 Float
    , radii : ( Float, Float )
    , startAngle : Float
    , deltaTheta : Float
    , xAxisRotate : Float
    }


type alias EndpointParameterization =
    { start : Vec2 Float
    , end : Vec2 Float
    , radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : Command.ArcFlag
    , direction : Command.Direction
    }


conversionMatrix : Float -> Mat2 Float
conversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, -1 * sin xAxisRotate )
    , ( sin xAxisRotate, cos xAxisRotate )
    )


inverseConversionMatrix : Float -> Mat2 Float
inverseConversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, sin xAxisRotate )
    , ( -1 * sin xAxisRotate, cos xAxisRotate )
    )


centerToEndpoint : CenterParameterization -> EndpointParameterization
centerToEndpoint { center, radii, startAngle, deltaTheta, xAxisRotate } =
    let
        conversion =
            conversionMatrix xAxisRotate

        endAngle =
            startAngle + deltaTheta

        ( rx, ry ) =
            radii

        p1 =
            ( rx * cos startAngle, ry * sin startAngle )
                |> Matrix2.mulVector conversion
                |> Vec2.add center

        p2 =
            ( rx * cos endAngle, ry * sin endAngle )
                |> Matrix2.mulVector conversion
                |> Vec2.add center

        ( arcFlag, direction ) =
            Path.LowLevel.decodeFlags
                ( if abs deltaTheta > pi then
                    1
                  else
                    0
                , if deltaTheta > 0 then
                    1
                  else
                    0
                )
                |> Maybe.withDefault ( smallestArc, counterClockwise )
    in
        { start = p1, end = p2, radii = radii, arcFlag = arcFlag, direction = direction, xAxisRotate = xAxisRotate }
