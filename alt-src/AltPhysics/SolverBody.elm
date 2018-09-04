module AltPhysics.SolverBody exposing
    ( SolverBody
    , addToWlambda
    , fromBody
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltMath.Vector4 as Vec4 exposing (Vec4)
import AltPhysics.Body as Body exposing (Body)
import AltPhysics.Const as Const
import AltPhysics.JacobianElement as JacobianElement exposing (JacobianElement)


type alias SolverBody =
    { position : Vec3
    , velocity : Vec3
    , angularVelocity : Vec3
    , quaternion : Vec4
    , force : Vec3
    , torque : Vec3
    , invMass : Float
    , invInertiaWorld : Mat4
    , vlambda : Vec3
    , wlambda : Vec3
    }


fromBody : Body -> SolverBody
fromBody { mass, position, velocity, angularVelocity, quaternion, force, torque, invMass, invInertiaWorld } =
    { position = position
    , velocity = velocity
    , angularVelocity = angularVelocity
    , quaternion = quaternion
    , force = force
    , torque = torque
    , invMass = invMass
    , invInertiaWorld = invInertiaWorld
    , vlambda = Const.zero3
    , wlambda = Const.zero3
    }


addToWlambda : Float -> JacobianElement -> SolverBody -> SolverBody
addToWlambda deltalambda { spatial, rotational } solverBody =
    { solverBody
        | vlambda =
            spatial
                |> Vec3.scale (deltalambda * solverBody.invMass)
                |> Vec3.add solverBody.vlambda
        , wlambda =
            rotational
                |> Mat4.transform solverBody.invInertiaWorld
                |> Vec3.scale deltalambda
                |> Vec3.add solverBody.wlambda
    }
