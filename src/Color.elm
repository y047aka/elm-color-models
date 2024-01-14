module Color exposing
    ( Color
    , rgb255, rgb, rgba, hsl, hsla
    , fromRgba, fromHsla
    , toCssString
    , toRgba, toHsla
    , rgbToHsl, hslToRgb
    )

{-|


# Types

@docs Color


## From numbers

@docs rgb255, rgb, rgba, hsl, hsla


## From records

@docs fromRgba, fromHsla


# Using colors with HTML/CSS/SVG

@docs toCssString


# Extracing values from colors

@docs toRgba, toHsla


#

@docs rgbToHsl, hslToRgb

-}


{-| Represents a color.
-}
type Color
    = Rgba Float Float Float Float
    | Hsla Float Float Float Float


fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba components =
    Rgba components.red components.green components.blue components.alpha


rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    Rgba r g b a


rgb : Float -> Float -> Float -> Color
rgb r g b =
    Rgba r g b 1.0


rgb255 : Int -> Int -> Int -> Color
rgb255 r g b =
    Rgba (scaleFrom255 r) (scaleFrom255 g) (scaleFrom255 b) 1.0


scaleFrom255 : Int -> Float
scaleFrom255 c =
    toFloat c / 255


fromHsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Color
fromHsla { hue, saturation, lightness, alpha } =
    Hsla hue saturation lightness alpha


hsla : Float -> Float -> Float -> Float -> Color
hsla hue sat light alpha =
    Hsla hue sat light alpha


hsl : Float -> Float -> Float -> Color
hsl h s l =
    hsla h s l 1.0


toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba c =
    case c of
        Rgba r g b a ->
            { red = r, green = g, blue = b, alpha = a }

        Hsla h s l a ->
            let
                ( red, green, blue ) =
                    hslToRgb h s l
            in
            { red = red, green = green, blue = blue, alpha = a }


toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla c =
    case c of
        Rgba r g b a ->
            let
                ( hue, saturation, lightness ) =
                    rgbToHsl r g b
            in
            { hue = hue, saturation = saturation, lightness = lightness, alpha = a }

        Hsla h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }


rgbToHsl : Float -> Float -> Float -> ( Float, Float, Float )
rgbToHsl r g b =
    let
        cMax =
            max (max r g) b

        cMin =
            min (min r g) b

        c =
            cMax - cMin

        h1 =
            if cMax == r then
                (g - b) / c

            else if cMax == g then
                ((b - r) / c) + 2

            else
                ((r - g) / c) + 4

        h2 =
            h1 * (1 / 6)

        h3 =
            if isNaN h2 then
                0

            else if h2 < 0 then
                h2 + 1

            else
                h2

        lightness =
            (cMax + cMin) / 2

        saturation =
            if lightness == 0 then
                0

            else
                c / (1 - abs (2 * lightness - 1))
    in
    ( h3, saturation, lightness )


hslToRgb : Float -> Float -> Float -> ( Float, Float, Float )
hslToRgb hue saturation lightness =
    let
        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        hue_ =
            hue * 360

        hueIsBetween lowerBound upperBound =
            lowerBound <= hue_ && hue_ <= upperBound

        zigUp xIntercept =
            chroma * (hue_ - xIntercept) / 60

        zigDown xIntercept =
            -1 * zigUp xIntercept

        ( r, g, b ) =
            if hueIsBetween 0 60 then
                ( chroma, zigUp 0, 0 )

            else if hueIsBetween 60 120 then
                ( zigDown 120, chroma, 0 )

            else if hueIsBetween 120 180 then
                ( 0, chroma, zigUp 120 )

            else if hueIsBetween 180 240 then
                ( 0, zigDown 240, chroma )

            else if hueIsBetween 240 300 then
                ( zigUp 240, 0, chroma )

            else
                ( chroma, 0, zigDown 360 )

        m =
            lightness - chroma / 2
    in
    ( r + m, g + m, b + m )


toCssString : Color -> String
toCssString c =
    let
        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    case c of
        Rgba r g b a ->
            cssFunction "rgba"
                [ String.fromFloat (pct r) ++ "%"
                , String.fromFloat (pct g) ++ "%"
                , String.fromFloat (pct b) ++ "%"
                , String.fromFloat (roundTo a)
                ]

        Hsla h s l a ->
            cssFunction "hsla"
                [ String.fromFloat (roundTo h)
                , String.fromFloat (pct s) ++ "%"
                , String.fromFloat (pct l) ++ "%"
                , String.fromFloat (roundTo a)
                ]


cssFunction : String -> List String -> String
cssFunction funcName args =
    funcName
        ++ "("
        ++ String.join "," args
        ++ ")"
