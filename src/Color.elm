module Color exposing
    ( Color
    , rgb255, rgb, rgba, hsl, hsla
    , fromRgba, fromHsla
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
toRgba (RgbaSpace r g b a) =
    { red = r, green = g, blue = b, alpha = a }


toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla (HslaSpace h s l a) =
    { hue = h, saturation = s, lightness = l, alpha = a }
