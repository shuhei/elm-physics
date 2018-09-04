module NarrowPhase exposing (main)

{- For a useful benchmark,
   copy and rename an older baseline version of Physics/NarrowPhase.elm
   to Physics/OriginalNarrowPhase.elm and uncomment the import below,
   then toggle the usage in benchmarks.

   Switching it back to use the (current) NarrowPhase.elm through the
   OriginalNarrowPhase alias keeps obsolete or redundant code out of
   the repo while the comparison benchmarks continue to be maintained and
   built and run essentially as absolute non-comparison benchmarks until
   they are needed again in another round of performance work.
-}
{- import Physics.OriginalNarrowPhase as OriginalNarrowPhase -}

import AltFixtures.ConvexPolyhedron as AltConvexPolyhedron
import AltFixtures.NarrowPhase
import AltMath.Vector3 as AltVec3
import AltPhysics.NarrowPhase as AltNarrowPhase
import AltPhysics.Quaternion as AltQuaternion
import AltPhysics.Transform as AltTransform
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Fixtures.ConvexPolyhedron as ConvexPolyhedron
import Fixtures.NarrowPhase
import Math.Vector3 as Vec3
import Physics.NarrowPhase as NarrowPhase
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        center =
            Vec3.vec3 0 0 7

        altCenter =
            AltVec3.vec3 0 0 7

        radius =
            5

        boxHalfExtent =
            1

        boxHull =
            ConvexPolyhedron.boxHull boxHalfExtent

        altBoxHull =
            AltConvexPolyhedron.boxHull boxHalfExtent

        boxPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions center radius boxHalfExtent
                |> List.map Tuple.first

        altBoxPositions =
            AltFixtures.NarrowPhase.sphereContactBoxPositions altCenter radius boxHalfExtent
                |> List.map Tuple.first

        boxFarPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions center (radius * 2) boxHalfExtent
                |> List.map Tuple.first

        altBoxFarPositions =
            AltFixtures.NarrowPhase.sphereContactBoxPositions altCenter (radius * 2) boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            3

        octoHull =
            ConvexPolyhedron.octoHull octoHalfExtent

        altOctoHull =
            AltConvexPolyhedron.originalOctoHull octoHalfExtent

        octoPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions center radius octoHalfExtent
                |> List.map Tuple.first

        altOctoPositions =
            AltFixtures.NarrowPhase.sphereContactOctohedronPositions altCenter radius octoHalfExtent
                |> List.map Tuple.first

        octoFarPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions center (radius * 2) octoHalfExtent
                |> List.map Tuple.first

        altOctoFarPositions =
            AltFixtures.NarrowPhase.sphereContactOctohedronPositions altCenter (radius * 2) octoHalfExtent
                |> List.map Tuple.first
    in
    describe "NarrowPhase"
        [ Benchmark.compare "addSphereConvexContacts box"
            "baseline"
            (\_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                Transform.identity
                                radius
                                0
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                boxHull
                                1
                                []
                        )
            )
            "latest code"
            (\_ ->
                altBoxPositions
                    |> List.map
                        (\position ->
                            AltNarrowPhase.addSphereConvexContacts
                                { position = altCenter
                                , quaternion = AltQuaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = AltQuaternion.identity
                                }
                                altBoxHull
                                1
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts box fail"
            "baseline"
            (\_ ->
                boxFarPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                boxHull
                                1
                                []
                        )
            )
            "latest code"
            (\_ ->
                altBoxFarPositions
                    |> List.map
                        (\position ->
                            AltNarrowPhase.addSphereConvexContacts
                                { position = altCenter
                                , quaternion = AltQuaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = AltQuaternion.identity
                                }
                                altBoxHull
                                1
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts octohedron"
            "baseline"
            (\_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                octoHull
                                1
                                []
                        )
            )
            "latest code"
            (\_ ->
                altOctoPositions
                    |> List.map
                        (\position ->
                            AltNarrowPhase.addSphereConvexContacts
                                { position = altCenter
                                , quaternion = AltQuaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = AltQuaternion.identity
                                }
                                altOctoHull
                                1
                                []
                        )
            )
        , Benchmark.compare "addSphereConvexContacts oct fail"
            "baseline"
            (\_ ->
                octoFarPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , quaternion = Quaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = Quaternion.identity
                                }
                                octoHull
                                1
                                []
                        )
            )
            "latest code"
            (\_ ->
                altOctoFarPositions
                    |> List.map
                        (\position ->
                            AltNarrowPhase.addSphereConvexContacts
                                { position = altCenter
                                , quaternion = AltQuaternion.identity
                                }
                                radius
                                0
                                { position = position
                                , quaternion = AltQuaternion.identity
                                }
                                altOctoHull
                                1
                                []
                        )
            )
        ]
