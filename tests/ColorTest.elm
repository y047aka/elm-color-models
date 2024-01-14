module ColorTest exposing (all)

import Color exposing (Color)
import CssHslReference
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, bool, floatRange, intRange)
import Hex
import Test exposing (..)


guaranteedTolerance =
    Absolute 0.0000000001



--
-- Fuzzers
--


unit : Fuzzer Float
unit =
    floatRange 0 1


int255 : Fuzzer Int
int255 =
    intRange 0 255


tuple2 : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
tuple2 a b =
    Fuzz.pair a b


tuple3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
tuple3 a b c =
    Fuzz.triple a b c


hex : Fuzzer Char
hex =
    Fuzz.oneOf (List.map Fuzz.constant <| String.toList "0123456789abcdefABCDEF")


hex2 : Fuzzer String
hex2 =
    Fuzz.map2 (\a b -> String.fromList [ a, b ]) hex hex



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
        , fuzz (tuple2 (tuple3 unit unit unit) unit)
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
        , fuzz (tuple2 (tuple3 unit unit unit) unit)
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
        , fuzz (tuple3 unit unit unit)
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
        , fuzz (tuple3 int255 int255 int255)
            "can represent RGB255 colors"
          <|
            \( r, g, b ) ->
                Color.rgb255 r g b
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within guaranteedTolerance (toFloat r / 255)
                        , .green >> Expect.within guaranteedTolerance (toFloat g / 255)
                        , .blue >> Expect.within guaranteedTolerance (toFloat b / 255)
                        , .alpha >> Expect.equal 1.0
                        ]
        , fuzz (tuple2 (tuple3 unit unit unit) unit)
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

                            else if h >= 1 then
                                result.hue |> Expect.within guaranteedTolerance (h - 1)

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
        , fuzz (tuple3 unit unit unit)
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

                            else if h >= 1 then
                                result.hue |> Expect.within guaranteedTolerance (h - 1)

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
        , fuzz (tuple2 (tuple3 unit unit unit) unit)
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

                            else if h >= 1 then
                                result.hue |> Expect.within guaranteedTolerance (h - 1)

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
        , fuzz (tuple2 (tuple3 (intRange 0 10000) (intRange 0 10000) (intRange 0 10000)) (intRange 0 1000))
            "can convert to CSS rgba strings"
          <|
            \( ( r, g, b ), a ) ->
                Color.rgba (toFloat r / 10000) (toFloat g / 10000) (toFloat b / 10000) (toFloat a / 1000)
                    |> Color.toCssString
                    |> Expect.equal
                        (String.concat
                            [ "rgba("
                            , String.fromFloat (toFloat r / 100)
                            , "%,"
                            , String.fromFloat (toFloat g / 100)
                            , "%,"
                            , String.fromFloat (toFloat b / 100)
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
                            Color.hsl info.h info.s info.l
                                |> Color.toRgba
                                |> Expect.all
                                    [ .red >> Expect.within guaranteedTolerance info.r
                                    , .green >> Expect.within guaranteedTolerance info.g
                                    , .blue >> Expect.within guaranteedTolerance info.b
                                    , .alpha >> Expect.equal 1.0
                                    ]

                testRgbToHsl i info =
                    test (String.fromInt i ++ ": " ++ Debug.toString info) <|
                        \() ->
                            Color.rgb info.r info.g info.b
                                |> Color.toHsla
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
                                    , .alpha >> Expect.equal 1.0
                                    ]
            in
            [ describe "HSL to RGB" <|
                List.indexedMap testHslToRgb CssHslReference.all
            , describe "RGB to HSL" <|
                List.indexedMap testRgbToHsl CssHslReference.all
            ]
        ]
