module DiagramCreation exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, float)
import Test exposing (..)
import ChordDiagram


{-
   chordCreation : Test
   chordCreation =
       describe "chord creation"
           [ fuzz (Fuzz.tuple ( float, float )) "empty list" <|
               \( padAngle, offset ) ->
                   ChordDiagram.chord { padAngle = padAngle, startAngle = offset } [] |> Expect.equal []
           , fuzz (Fuzz.tuple ( float, float )) "singleton list" <|
               \( v, offset ) ->
                   ChordDiagram.chord { padAngle = pi, startAngle = offset } [ [ v ] ]
                       |> Expect.equal
                           [ { source = { startAngle = offset, endAngle = 3.1415926535897936, value = v }
                             , target = { startAngle = offset, endAngle = 3.1415926535897936, value = v }
                             }
                           ]
           ]
-}


chordAtom : Test
chordAtom =
    fuzz float "chord atom" <|
        \k ->
            ChordDiagram.atom k 0 1
                |> Expect.equal ( k, { startAngle = 0, endAngle = k, value = 1 } )


chordSubgroup : Test
chordSubgroup =
    test "chord subgroup" <|
        \_ ->
            let
                k =
                    2 * pi
            in
                ChordDiagram.partitionSubgroup k 0 [ 0.2, 0.8 ]
                    |> Expect.equal
                        ( k
                        , 1
                        , [ { startAngle = 0.2 * 2 * pi, endAngle = 2 * pi, value = 0.8 }
                          , { startAngle = 0, endAngle = 0.2 * 2 * pi, value = 0.2 }
                          ]
                        )


chordGroup : Test
chordGroup =
    test "chord group" <|
        \_ ->
            let
                k =
                    2 * pi
            in
                ChordDiagram.partitionGroup k 0 0 [ [ 0.1, 0.4 ], [ 0.25, 0.25 ] ]
                    |> Expect.equal
                        ( k
                        , 1
                        , [ { startAngle = 3.141592653589793
                            , endAngle = 6.283185307179586
                            , connections =
                                [ { startAngle = 3.7699111843077517, endAngle = 6.283185307179586, value = 0.4 }
                                , { startAngle = 3.141592653589793, endAngle = 3.7699111843077517, value = 0.1 }
                                ]
                            , value = 0.5
                            }
                          , { startAngle = 0
                            , endAngle = 3.141592653589793
                            , connections =
                                [ { startAngle = 1.5707963267948966, endAngle = 3.141592653589793, value = 0.25 }
                                , { startAngle = 0, endAngle = 1.5707963267948966, value = 0.25 }
                                ]
                            , value = 0.5
                            }
                          ]
                        )


(=>) =
    (,)


endToEndUnit : Test
endToEndUnit =
    let
        matrix =
            [ "A" => [ 0, 1, 1 ]
            , "B" => [ 1, 0, 1 ]
            , "Ð" => [ 1, 1, 0 ]
            ]

        ( _, numbers ) =
            List.unzip matrix

        groups =
            numbers
                |> ChordDiagram.chord { padAngle = 0, startAngle = 0 }

        expected =
            [ { startAngle = 2.617993877991494
              , endAngle = 4.712388980384689
              , value = 2
              , connections =
                    [ { source = { startAngle = 5.235987755982988, endAngle = 6.283185307179585, value = 1 }, target = { startAngle = 0, endAngle = 1.0471975511965976, value = 1 } }
                    , { source = { startAngle = 4.1887902047863905, endAngle = 5.235987755982988, value = 1 }, target = { startAngle = 2.0943951023931953, endAngle = 3.141592653589793, value = 1 } }
                    ]
              }
            , { startAngle = 0.5235987755982987
              , endAngle = 2.617993877991494
              , value = 2
              , connections =
                    [ { source = { startAngle = 3.141592653589793, endAngle = 4.1887902047863905, value = 1 }, target = { startAngle = 1.0471975511965976, endAngle = 2.0943951023931953, value = 1 } }
                    ]
              }
            , { startAngle = -1.5707963267948966, endAngle = 0.5235987755982987, value = 2, connections = [] }
            ]
    in
        test "end to end unit test of chord creation" <|
            \_ ->
                groups
                    |> Expect.equal expected
