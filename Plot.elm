module Plot exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, normalize, cross, sub)
import Color exposing (Color)
import WebGL exposing (Mesh, Shader)
import Shapes exposing (cone, cylinder)
import SceneGraph exposing (asRenderable, toEntities, SceneGraph)


--TODO: implement proper normal transformation


plot : ( Int, Int ) -> Html msg
plot ( x, y ) =
    WebGL.toHtml
        [ width 400
        , height 400
        , style [ ( "display", "block" ) ]
        ]
        (toEntities <| scene ( x, y ))


scene : ( Int, Int ) -> SceneGraph
scene ( x, y ) =
    SceneGraph.Transform viewMatrix
        [ SceneGraph.Transform (modelTransform ( x, y ))
            [ arrow 4 0.1 ]
        ]


arrow : Float -> Float -> SceneGraph
arrow length thickness =
    let
        coneHeight =
            5 * thickness

        coneThickness =
            2.5 * thickness

        tailHeight =
            max 0 (length - coneHeight)

        coneTransform =
            Mat4.makeTranslate3 0 0 (tailHeight / 2)
                |> Mat4.scale3 coneThickness coneThickness coneHeight

        cylinderTransform =
            Mat4.makeTranslate3 0 0 (-coneHeight / 2)
                |> Mat4.scale3 thickness thickness tailHeight

        renderable tris =
            asRenderable
                { mesh = WebGL.triangles tris
                , vertexShader = vertexShader
                , fragmentShader = fragmentShader
                , uniforms =
                    (\transform ->
                        { perspective = perspectiveMatrix
                        , color = toVec3 Color.blue
                        , transform = transform
                        }
                    )
                }
    in
        SceneGraph.Transform Mat4.identity
            [ SceneGraph.Object coneTransform (renderable <| cone 16) []
            , SceneGraph.Object cylinderTransform (renderable <| cylinder 8) []
            ]


modelTransform : ( Int, Int ) -> Mat4
modelTransform ( x, y ) =
    Mat4.makeRotate (toFloat y / 100) Vec3.i
        |> Mat4.rotate (toFloat -x / 100) Vec3.j


perspectiveMatrix : Mat4
perspectiveMatrix =
    (Mat4.makePerspective 45 1 0.01 100)


viewMatrix : Mat4
viewMatrix =
    (Mat4.makeLookAt (vec3 0 0 8) (vec3 0 0 0) (vec3 0 1 0))


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


toVec3 : Color -> Vec3
toVec3 color =
    let
        { red, green, blue } =
            Color.toRgb color

        normalize n =
            toFloat n / 255

        ( redFloat, greenFloat, blueFloat ) =
            ( normalize red, normalize green, normalize blue )
    in
        vec3 redFloat greenFloat blueFloat



-- Shaders


type alias Uniforms =
    { perspective : Mat4
    , transform : Mat4
    , color : Vec3
    }


type alias Varying =
    { vlighting : Float
    }


vertexShader : Shader Vertex Uniforms Varying
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 perspective;
        uniform mat4 transform;

        varying float vlighting;

        float ambientStrength = 0.4;
        float directionalStrength = 0.6;
        vec3 directionalVector = vec3(0, 1, 0);

        void main () {
            gl_Position = perspective * transform * vec4(position, 1.0);
            vec4 transformedNormal = normalize (transform * vec4(normal, 1.0));
            float directional = directionalStrength * max(dot(transformedNormal.xyz, directionalVector), 0.0);
            vlighting = directional + ambientStrength;
        }
    |]


fragmentShader : Shader {} Uniforms Varying
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying highp float vlighting;

        void main () {
            gl_FragColor = vec4(color*vlighting, 1.0);
        }
    |]
