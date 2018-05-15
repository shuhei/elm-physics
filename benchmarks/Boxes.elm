module BoxesBenchmark exposing (main)

import Physics
import Benchmark exposing (..)
import Math.Vector3 exposing (vec3)
import Benchmark.Runner exposing (BenchmarkProgram, program)


main : BenchmarkProgram
main =
    (\_ -> Physics.step (1 / 60) world)
        |> benchmark "Physics.step 4x4x4 boxes falling on the plane"
        |> program


world : Physics.World
world =
    Physics.world
        |> Physics.setGravity (vec3 0 0 -10)
        |> Physics.addBody
            (Physics.body
                |> Physics.addShape Physics.plane
                |> Physics.offsetBy (vec3 0 0 -1)
            )
        |> range3 addBoxAt 4 2.5


addBoxAt : Float -> Float -> Float -> Physics.World -> Physics.World
addBoxAt x y z =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))
        |> Physics.offsetBy (vec3 x y z)
        |> Physics.addBody


range3 : (Float -> Float -> Float -> a -> a) -> Int -> Float -> a -> a
range3 fn size distance init =
    List.foldl
        (\x acc1 ->
            List.foldl
                (\y acc2 ->
                    List.foldl
                        (\z acc3 ->
                            fn
                                ((toFloat x - toFloat (size - 1) / 2) * distance)
                                ((toFloat y - toFloat (size - 1) / 2) * distance)
                                (toFloat z * distance)
                                acc3
                        )
                        acc2
                        (List.range 0 (size - 1))
                )
                acc1
                (List.range 0 (size - 1))
        )
        init
        (List.range 0 (size - 1))
