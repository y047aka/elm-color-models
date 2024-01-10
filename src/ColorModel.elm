module ColorModel exposing
    ( Color(..)
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

import Color


{-| Represents a color.
-}
type Color
    = RgbaSpace Float Float Float Float
    | HslaSpace Float Float Float Float


fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba components =
    RgbaSpace components.red components.green components.blue components.alpha


rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RgbaSpace r g b a


rgb : Float -> Float -> Float -> Color
rgb r g b =
    RgbaSpace r g b 1.0


rgb255 : Int -> Int -> Int -> Color
rgb255 r g b =
    RgbaSpace (scaleFrom255 r) (scaleFrom255 g) (scaleFrom255 b) 1.0


scaleFrom255 : Int -> Float
scaleFrom255 c =
    toFloat c / 255


fromHsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Color
fromHsla { hue, saturation, lightness, alpha } =
    HslaSpace hue saturation lightness alpha


hsla : Float -> Float -> Float -> Float -> Color
hsla hue sat light alpha =
    HslaSpace hue sat light alpha


hsl : Float -> Float -> Float -> Color
hsl h s l =
    hsla h s l 1.0


toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba c =
    case c of
        RgbaSpace r g b a ->
            { red = r, green = g, blue = b, alpha = a }

        HslaSpace h s l a ->
            hslaToRgba h s l a


toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla c =
    case c of
        RgbaSpace r g b a ->
            rgbaToHsla r g b a

        HslaSpace h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }


rgbaToHsla : Float -> Float -> Float -> Float -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
rgbaToHsla r g b a =
    Color.toHsla (Color.rgba r g b a)


hslaToRgba : Float -> Float -> Float -> Float -> { red : Float, green : Float, blue : Float, alpha : Float }
hslaToRgba h s l a =
    Color.toRgba (Color.hsla h s l a)


toCssString : Color -> String
toCssString c =
    let
        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    case c of
        RgbaSpace r g b a ->
            cssFunction "rgba"
                [ String.fromFloat (pct r) ++ "%"
                , String.fromFloat (pct g) ++ "%"
                , String.fromFloat (pct b) ++ "%"
                , String.fromFloat (roundTo a)
                ]

        HslaSpace h s l a ->
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
