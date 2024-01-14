module Color exposing
    ( Color
    , rgb255, rgb, rgba, hsl, hsla
    , fromRgba, fromHsla
    , toCssString
    , toRgba, toHsla
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
            hslaToRgba h s l a


toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla c =
    case c of
        Rgba r g b a ->
            rgbaToHsla r g b a

        Hsla h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }


rgbaToHsla : Float -> Float -> Float -> Float -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
rgbaToHsla r g b a =
    toHsla_ (rgba r g b a)


hslaToRgba : Float -> Float -> Float -> Float -> { red : Float, green : Float, blue : Float, alpha : Float }
hslaToRgba h s l a =
    toRgba (hsla_ h s l a)


hsla_ : Float -> Float -> Float -> Float -> Color
hsla_ hue sat light alpha =
    let
        ( h, s, l ) =
            ( hue, sat, light )

        m2 =
            if l <= 0.5 then
                l * (s + 1)

            else
                l + s - l * s

        m1 =
            l * 2 - m2

        r =
            hueToRgb (h + 1 / 3)

        g =
            hueToRgb h

        b =
            hueToRgb (h - 1 / 3)

        hueToRgb h__ =
            let
                h_ =
                    if h__ < 0 then
                        h__ + 1

                    else if h__ > 1 then
                        h__ - 1

                    else
                        h__
            in
            if h_ * 6 < 1 then
                m1 + (m2 - m1) * h_ * 6

            else if h_ * 2 < 1 then
                m2

            else if h_ * 3 < 2 then
                m1 + (m2 - m1) * (2 / 3 - h_) * 6

            else
                m1
    in
    Rgba r g b alpha


toHsla_ : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla_ c =
    case c of
        Rgba r g b a ->
            let
                minColor =
                    min r (min g b)

                maxColor =
                    max r (max g b)

                h1 =
                    if maxColor == r then
                        (g - b) / (maxColor - minColor)

                    else if maxColor == g then
                        2 + (b - r) / (maxColor - minColor)

                    else
                        4 + (r - g) / (maxColor - minColor)

                h2 =
                    h1 * (1 / 6)

                h3 =
                    if isNaN h2 then
                        0

                    else if h2 < 0 then
                        h2 + 1

                    else
                        h2

                l =
                    (minColor + maxColor) / 2

                s =
                    if minColor == maxColor then
                        0

                    else if l < 0.5 then
                        (maxColor - minColor) / (maxColor + minColor)

                    else
                        (maxColor - minColor) / (2 - maxColor - minColor)
            in
            { hue = h3
            , saturation = s
            , lightness = l
            , alpha = a
            }

        Hsla h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }


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
