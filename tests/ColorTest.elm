module ColorTest exposing (all)

import Color exposing (Color)
import CssHslReference
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, floatRange, intRange, pair, triple)
import Test exposing (..)


guaranteedTolerance : FloatingPointTolerance
guaranteedTolerance =
    Absolute 0.0000000001



--
-- Fuzzers
--


unit : Fuzzer Float
unit =
    floatRange 0 1


rgbValues : Fuzzer ( Float, Float, Float )
rgbValues =
    let
        float255 =
            intRange 0 25500 |> Fuzz.map (toFloat >> (\n -> n / 100))
    in
    triple float255 float255 float255


rgbaValues : Fuzzer a -> Fuzzer ( ( Float, Float, Float ), a )
rgbaValues alpha =
    pair rgbValues alpha


hslValues : Fuzzer ( Float, Float, Float )
hslValues =
    let
        float360 =
            intRange 0 36000 |> Fuzz.map (toFloat >> (\n -> n / 100))
    in
    triple float360 unit unit


hslaValues : Fuzzer ( ( Float, Float, Float ), Float )
hslaValues =
    pair hslValues opacityValue


opacityValue : Fuzzer Float
opacityValue =
    floatRange 0 1



--
-- Tests
--


all : Test
all =
    describe "Color"
        [ test "defines a Color type" <|
            \() ->
                let
                    color : Color
                    color =
                        Color.rgba 0 0 0 0
                in
                color
                    |> Expect.equal color
        , fuzz (rgbaValues opacityValue)
            "can represent RGBA colors (fromRgba)"
          <|
            \( ( r, g, b ), a ) ->
                Color.fromRgba { red = r, green = g, blue = b, alpha = a }
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within guaranteedTolerance r
                        , .green >> Expect.within guaranteedTolerance g
                        , .blue >> Expect.within guaranteedTolerance b
                        , .alpha >> Expect.within guaranteedTolerance a
                        ]
        , fuzz (rgbaValues opacityValue)
            "can represent RGBA colors (rgba)"
          <|
            \( ( r, g, b ), a ) ->
                Color.rgba r g b a
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within guaranteedTolerance r
                        , .green >> Expect.within guaranteedTolerance g
                        , .blue >> Expect.within guaranteedTolerance b
                        , .alpha >> Expect.within guaranteedTolerance a
                        ]
        , fuzz rgbValues
            "can represent RGBA colors (rgb)"
          <|
            \( r, g, b ) ->
                Color.rgb r g b
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within guaranteedTolerance r
                        , .green >> Expect.within guaranteedTolerance g
                        , .blue >> Expect.within guaranteedTolerance b
                        , .alpha >> Expect.equal 1.0
                        ]
        , fuzz hslaValues
            "can represent HSLA colors (fromHsla)"
          <|
            \( ( h, s, l ), a ) ->
                Color.fromHsla { hue = h, saturation = s, lightness = l, alpha = a }
                    |> Color.toHsla
                    |> Expect.all
                        [ \result ->
                            if result.lightness == 1 || result.lightness == 0 || result.saturation == 0 then
                                -- hue does not apply
                                Expect.pass

                            else if (h / 360) > 1 then
                                result.hue |> Expect.within guaranteedTolerance (h - 360)

                            else
                                result.hue |> Expect.within guaranteedTolerance h
                        , \result ->
                            if result.lightness == 1 || result.lightness == 0 then
                                -- saturation does not apply
                                Expect.pass

                            else
                                result.saturation |> Expect.within guaranteedTolerance s
                        , .lightness >> Expect.within guaranteedTolerance l
                        , .alpha >> Expect.within guaranteedTolerance a
                        ]
        , fuzz hslValues
            "can represent HSLA colors (hsl)"
          <|
            \( h, s, l ) ->
                Color.hsl h s l
                    |> Color.toHsla
                    |> Expect.all
                        [ \result ->
                            if result.lightness == 1 || result.lightness == 0 || result.saturation == 0 then
                                -- hue does not apply
                                Expect.pass

                            else if (h / 360) > 1 then
                                result.hue |> Expect.within guaranteedTolerance (h - 360)

                            else
                                result.hue |> Expect.within guaranteedTolerance h
                        , \result ->
                            if result.lightness == 1 || result.lightness == 0 then
                                -- saturation does not apply
                                Expect.pass

                            else
                                result.saturation |> Expect.within guaranteedTolerance s
                        , .lightness >> Expect.within guaranteedTolerance l
                        , .alpha >> Expect.equal 1.0
                        ]
        , fuzz hslaValues
            "can represent HSLA colors (hsla)"
          <|
            \( ( h, s, l ), a ) ->
                Color.hsla h s l a
                    |> Color.toHsla
                    |> Expect.all
                        [ \result ->
                            if result.lightness == 1 || result.lightness == 0 || result.saturation == 0 then
                                -- hue does not apply
                                Expect.pass

                            else if (h / 360) > 1 then
                                result.hue |> Expect.within guaranteedTolerance (h - 360)

                            else
                                result.hue |> Expect.within guaranteedTolerance h
                        , \result ->
                            if result.lightness == 1 || result.lightness == 0 then
                                -- saturation does not apply
                                Expect.pass

                            else
                                result.saturation |> Expect.within guaranteedTolerance s
                        , .lightness >> Expect.within guaranteedTolerance l
                        , .alpha >> Expect.within guaranteedTolerance a
                        ]
        , fuzz (rgbaValues (intRange 0 1000))
            "can convert to CSS rgba strings"
          <|
            \( ( r, g, b ), a ) ->
                Color.rgba r g b (toFloat a / 1000)
                    |> Color.toCssString
                    |> Expect.equal
                        (String.concat
                            [ "rgba("
                            , String.fromFloat r
                            , ","
                            , String.fromFloat g
                            , ","
                            , String.fromFloat b
                            , ","
                            , String.fromFloat (toFloat a / 1000)
                            , ")"
                            ]
                        )
        , fuzz (pair (triple (intRange 0 360) (intRange 0 100) (intRange 0 100)) (intRange 0 1000))
            "can convert to CSS hsla strings"
          <|
            \( ( h, s, l ), a ) ->
                Color.hsla (toFloat h / 100) (toFloat s / 100) (toFloat l / 100) (toFloat a / 1000)
                    |> Color.toCssString
                    |> Expect.equal
                        (String.concat
                            [ "hsla("
                            , String.fromFloat (toFloat h / 100)
                            , ","
                            , String.fromFloat (toFloat s)
                            , "%,"
                            , String.fromFloat (toFloat l)
                            , "%,"
                            , String.fromFloat (toFloat a / 1000)
                            , ")"
                            ]
                        )
        , describe "color reference" <|
            let
                testHslToRgb i info =
                    test (String.fromInt i ++ ": " ++ Debug.toString info) <|
                        \() ->
                            Color.hslToRgb (info.h * 360) info.s info.l
                                |> (\( r, g, b ) -> { red = r / 255, green = g / 255, blue = b / 255 })
                                |> Expect.all
                                    [ .red >> Expect.within guaranteedTolerance info.r
                                    , .green >> Expect.within guaranteedTolerance info.g
                                    , .blue >> Expect.within guaranteedTolerance info.b
                                    ]

                testRgbToHsl i info =
                    test (String.fromInt i ++ ": " ++ Debug.toString info) <|
                        \() ->
                            Color.rgbToHsl (info.r * 255) (info.g * 255) (info.b * 255)
                                |> (\( h, s, l ) -> { hue = h / 360, saturation = s, lightness = l })
                                |> Expect.all
                                    [ if info.l == 1 || info.l == 0 || info.s == 0 then
                                        -- hue does not apply
                                        always Expect.pass

                                      else if info.h >= 1 then
                                        .hue >> Expect.within guaranteedTolerance (info.h - 1)

                                      else
                                        .hue >> Expect.within guaranteedTolerance info.h
                                    , if info.l == 1 || info.l == 0 then
                                        -- saturation does not apply
                                        always Expect.pass

                                      else
                                        .saturation >> Expect.within guaranteedTolerance info.s
                                    , .lightness >> Expect.within guaranteedTolerance info.l
                                    ]
            in
            [ describe "HSL to RGB" <|
                List.indexedMap testHslToRgb CssHslReference.all
            , describe "RGB to HSL" <|
                List.indexedMap testRgbToHsl CssHslReference.all
            ]
        ]
