module Complex exposing (..)


type alias Complex =
    { real : Float
    , imaginary : Float
    }


type alias PolarComplex =
    { r : Float
    , theta : Float
    }


i : Complex
i =
    Complex 0 1


toTuple : Complex -> ( Float, Float )
toTuple complex =
    ( complex.real, complex.imaginary )


fromTuple : ( Float, Float ) -> Complex
fromTuple ( real, imaginary ) =
    Complex real imaginary


add : Complex -> Complex -> Complex
add a b =
    Complex (a.real + b.real) (a.imaginary + b.imaginary)


subtract : Complex -> Complex -> Complex
subtract a b =
    Complex (a.real - b.real) (a.imaginary - b.imaginary)


multiply : Complex -> Complex -> Complex
multiply n1 n2 =
    let
        ( a, b ) =
            toTuple n1

        ( c, d ) =
            toTuple n2
    in
        Complex (a * c - b * d) (a * d + b * c)


power : Complex -> Complex -> Complex
power n1 n2 =
    let
        ( a, b ) =
            toTuple n1

        ( c, d ) =
            toTuple n2

        ln =
            logBase e

        scale =
            (a ^ 2 + b ^ 2) ^ (c / 2) * e ^ (-d * arg n1)

        theta =
            (c * (arg n1) + 0.5 * d * ln (norm n1))

        real =
            scale * cos theta

        imaginary =
            scale * sin theta
    in
        Complex real imaginary


divide : Complex -> Complex -> Complex
divide n1 n2 =
    let
        ( a, b ) =
            toTuple n1

        ( c, d ) =
            toTuple n2

        denom =
            c ^ 2 + d ^ 2

        real =
            (a * c + b * d) / denom

        imaginary =
            (b * c - a * d) / denom
    in
        Complex real imaginary


conjugate : Complex -> Complex
conjugate { real, imaginary } =
    Complex real -imaginary


abs : Complex -> Float
abs { real, imaginary } =
    sqrt (real ^ 2 + imaginary ^ 2)


arg : Complex -> Float
arg { real, imaginary } =
    atan2 imaginary real


norm : Complex -> Float
norm { real, imaginary } =
    real ^ 2 + imaginary ^ 2


toPolar : Complex -> PolarComplex
toPolar { real, imaginary } =
    let
        ( r, theta ) =
            Basics.toPolar ( real, imaginary )
    in
        PolarComplex r theta


fromPolar : PolarComplex -> Complex
fromPolar { r, theta } =
    fromTuple <| Basics.fromPolar ( r, theta )


fromReal : Float -> Complex
fromReal real =
    Complex real 0



--fromString : String -> Complex
--sin : Complex -> Complex
--cos : Complex -> Complex
--tan : Complex -> Complex
--sec : Complex -> Complex
--csc : Complex -> Complex
--cot : Complex -> Complex
