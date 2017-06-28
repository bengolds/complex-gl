module SceneGraph exposing (toEntities, Renderable, SceneGraph(..), asRenderable, RenderInfo)

import WebGL exposing (Mesh, Shader)
import Math.Matrix4 as Mat4 exposing (Mat4)


type alias Renderable =
    Mat4 -> WebGL.Entity


type SceneGraph
    = Object Mat4 Renderable (List SceneGraph)
    | Transform Mat4 (List SceneGraph)
    | Empty


toEntities : SceneGraph -> List WebGL.Entity
toEntities root =
    toEntitiesHelper Mat4.identity root


toEntitiesHelper : Mat4 -> SceneGraph -> List WebGL.Entity
toEntitiesHelper transform node =
    case node of
        Empty ->
            []

        Transform innerTransform children ->
            List.concatMap (toEntitiesHelper (Mat4.mul transform innerTransform)) children

        Object innerTransform renderable children ->
            (renderable (Mat4.mul transform innerTransform))
                :: List.concatMap (toEntitiesHelper (Mat4.mul transform innerTransform)) children


type alias WithTransform a =
    { a | transform : Mat4 }


type alias RenderInfo attributes uniforms varyings =
    { mesh : Mesh attributes
    , vertexShader : Shader attributes uniforms varyings
    , fragmentShader : Shader {} uniforms varyings
    , uniforms : Mat4 -> uniforms
    }


asRenderable : RenderInfo a u v -> (Mat4 -> WebGL.Entity)
asRenderable { mesh, vertexShader, fragmentShader, uniforms } =
    (\transform ->
        WebGL.entity vertexShader
            fragmentShader
            mesh
            (uniforms transform)
    )
