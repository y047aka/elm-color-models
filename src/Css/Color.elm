module Css.Color exposing (backgroundColor, borderColor, color)

import Color exposing (Color(..))
import Css exposing (Style, property)


cssFunction : String -> List String -> String
cssFunction funcName args =
    funcName
        ++ "("
        ++ String.join "," args
        ++ ")"


backgroundColor : Color -> Style
backgroundColor c =
    let
        value =
            case c of
                RgbaSpace r g b alpha ->
                    cssFunction "rgba" (List.map String.fromFloat [ r, g, b, alpha ])

                HslaSpace h s l alpha ->
                    cssFunction "hsla" (List.map String.fromFloat [ h, s, l, alpha ])
    in
    property "background-color" value


color : Color -> Style
color c =
    let
        value =
            case c of
                RgbaSpace r g b alpha ->
                    cssFunction "rgba" (List.map String.fromFloat [ r, g, b, alpha ])

                HslaSpace h s l alpha ->
                    cssFunction "hsla" (List.map String.fromFloat [ h, s, l, alpha ])
    in
    property "color" value


borderColor : Color -> Style
borderColor c =
    let
        value =
            case c of
                RgbaSpace r g b alpha ->
                    cssFunction "rgba" (List.map String.fromFloat [ r, g, b, alpha ])

                HslaSpace h s l alpha ->
                    cssFunction "hsla" (List.map String.fromFloat [ h, s, l, alpha ])
    in
    property "border-color" value
