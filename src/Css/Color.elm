module Css.Color exposing (backgroundColor, borderColor, color)

import ColorModel exposing (Color(..))
import Css exposing (Style, property)


cssFunction : String -> List String -> String
cssFunction funcName args =
    funcName
        ++ "("
        ++ String.join "," args
        ++ ")"


cssValue : Color -> String
cssValue c =
    case c of
        RgbaSpace r g b alpha ->
            cssFunction "rgba" (List.map String.fromFloat [ r, g, b, alpha ])

        HslaSpace h s l alpha ->
            cssFunction "hsla" (List.map String.fromFloat [ h, s, l, alpha ])


backgroundColor : Color -> Style
backgroundColor c =
    property "background-color" (cssValue c)


color : Color -> Style
color c =
    property "color" (cssValue c)


borderColor : Color -> Style
borderColor c =
    property "border-color" (cssValue c)
