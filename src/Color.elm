module Color exposing
    ( Color
    , rgb, rgba, hsl, hsla
    , fromRgba, fromHsla
    , toCssString
    , toRgba, toHsla
    , rgbToHsl, hslToRgb
    )

{-|


# Types

@docs Color


## From numbers

@docs rgb, rgba, hsl, hsla


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
    = Rgba255 Float Float Float Float
    | Hsla360 Float Float Float Float


fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba components =
    Rgba255 components.red components.green components.blue components.alpha


rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    Rgba255 r g b a


rgb : Float -> Float -> Float -> Color
rgb r g b =
    Rgba255 r g b 1.0


fromHsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Color
fromHsla { hue, saturation, lightness, alpha } =
    Hsla360 hue saturation lightness alpha


hsla : Float -> Float -> Float -> Float -> Color
hsla hue sat light alpha =
    Hsla360 hue sat light alpha


hsl : Float -> Float -> Float -> Color
hsl h s l =
    hsla h s l 1.0


toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba c =
    case c of
        Rgba255 r g b a ->
            { red = r, green = g, blue = b, alpha = a }

        Hsla360 h s l a ->
            let
                ( red, green, blue ) =
                    hslToRgb h s l
            in
            { red = red, green = green, blue = blue, alpha = a }


toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla c =
    case c of
        Rgba255 r g b a ->
            let
                ( hue, saturation, lightness ) =
                    rgbToHsl r g b
            in
            { hue = hue, saturation = saturation, lightness = lightness, alpha = a }

        Hsla360 h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }


rgbToHsl : Float -> Float -> Float -> ( Float, Float, Float )
rgbToHsl red green blue =
    let
        ( r, g, b ) =
            ( red / 255, green / 255, blue / 255 )

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
    ( h3 * 360, saturation, lightness )


hslToRgb : Float -> Float -> Float -> ( Float, Float, Float )
hslToRgb hue saturation lightness =
    let
        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        hueIsBetween lowerBound upperBound =
            lowerBound <= hue && hue <= upperBound

        zigUp xIntercept =
            chroma * (hue - xIntercept) / 60

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
    ( (r + m) * 255, (g + m) * 255, (b + m) * 255 )


toCssString : Color -> String
toCssString c =
    let
        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    case c of
        Rgba255 r g b a ->
            cssFunction "rgba"
                [ String.fromFloat (roundTo r)
                , String.fromFloat (roundTo g)
                , String.fromFloat (roundTo b)
                , String.fromFloat (roundTo a)
                ]

        Hsla360 h s l a ->
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
