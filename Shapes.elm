module Shapes exposing (cone, cylinder, Vertex)

import Math.Vector3 as Vec3 exposing (Vec3, vec3, normalize, cross, sub)


-- UTILS


type alias Triangle a =
    ( a, a, a )


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


mapAdjacent : (a -> a -> b) -> List a -> List b
mapAdjacent fn list =
    List.map2 fn
        list
        (List.tail list |> Maybe.withDefault [])


normal : Triangle Vec3 -> Vec3
normal ( v1, v2, v3 ) =
    normalize (cross (sub v1 v2) (sub v1 v3))


addFaceNormals : Triangle Vec3 -> Triangle Vertex
addFaceNormals ( v1, v2, v3 ) =
    let
        n =
            normal ( v1, v2, v3 )
    in
        ( { position = v1, normal = n }
        , { position = v2, normal = n }
        , { position = v3, normal = n }
        )


circle : Int -> Float -> List ( Float, Float )
circle numPoints radius =
    let
        step =
            2 * pi / (toFloat numPoints)
    in
        List.range 0 numPoints
            |> List.map toFloat
            |> List.map ((*) step)
            |> List.map (\theta -> ( radius * cos theta, radius * sin theta ))



-- SHAPES


cone : Int -> List (Triangle Vertex)
cone faces =
    let
        topPoint =
            vec3 0 0 0.5

        bottomPoint =
            vec3 0 0 -0.5
    in
        circle faces 1
            |> List.map (\( x, y ) -> vec3 x y -0.5)
            |> mapAdjacent (\v1 v2 -> [ ( v1, topPoint, v2 ), ( v1, bottomPoint, v2 ) ])
            |> List.concat
            |> List.map addFaceNormals


cylinder : Int -> List (Triangle Vertex)
cylinder faces =
    circle faces 1
        |> mapAdjacent
            (\( x1, y1 ) ( x2, y2 ) ->
                let
                    topPoint =
                        vec3 0 0 0.5

                    bottomPoint =
                        vec3 0 0 -0.5

                    bottomRight =
                        vec3 x1 y1 -0.5

                    topRight =
                        vec3 x1 y1 0.5

                    bottomLeft =
                        vec3 x2 y2 -0.5

                    topLeft =
                        vec3 x2 y2 0.5
                in
                    [ ( bottomRight, topRight, bottomLeft )
                    , ( topRight, topLeft, bottomLeft )
                    , ( topRight, topPoint, topLeft )
                    , ( bottomRight, bottomLeft, bottomPoint )
                    ]
            )
        |> List.concat
        |> List.map addFaceNormals
