module Css.Color exposing (backgroundColor, borderColor, color)

import ColorModel exposing (Color(..), toCssString)
import Css exposing (Style, property)


backgroundColor : Color -> Style
backgroundColor c =
    property "background-color" (toCssString c)


color : Color -> Style
color c =
    property "color" (toCssString c)


borderColor : Color -> Style
borderColor c =
    property "border-color" (toCssString c)
