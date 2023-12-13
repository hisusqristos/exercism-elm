module ComplexNumbers exposing
    ( Complex
    , abs
    , add
    , conjugate
    , div
    , exp
    , fromPair
    , fromReal
    , imaginary
    , mul
    , real
    , sub
    )


type alias Complex =
    ( Float, Float )


i : number
i =
    1



-- i = 1i but this notation causes infinite loop


fromPair : ( Float, Float ) -> Complex
fromPair z =
    z


fromReal : Float -> Complex
fromReal float =
    ( float, 0 * i )


real : Complex -> Float
real =
    Tuple.first


imaginary : Complex -> Float
imaginary =
    Tuple.second


conjugate : Complex -> Complex
conjugate =
    Tuple.mapSecond negate


abs : Complex -> Float
abs ( a, bi ) =
    sqrt (a ^ 2 + bi ^ 2)


add : Complex -> Complex -> Complex
add ( a, bi ) =
    Tuple.mapBoth ((+) a) ((+) bi)


sub : Complex -> Complex -> Complex
sub ( a, bi ) =
    Tuple.mapBoth ((-) a) ((-) bi)


mul : Complex -> Complex -> Complex
mul ( a, bi ) ( c, di ) =
    (bi * c + a * di)
        |> Tuple.pair (a * c - bi * di)


div : Complex -> Complex -> Complex
div ( a, b ) ( c, d ) =
    ((b * c - a * d) / (c ^ 2 + d ^ 2) * i)
        |> Tuple.pair ((a * c + b * d) / (c ^ 2 + d ^ 2))


exp : Complex -> Complex
exp ( a, b ) =
    ( (e ^ a) * cos b, (e ^ a) * sin b )
