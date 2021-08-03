module Theme exposing (column, layout, priorityBadge, row)

import Color exposing (Color)
import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes exposing (class, style)
import Route


layout : String -> Html msg -> Html msg
layout title body =
    row [ padding ]
        [ sidebar
        , div [ style "margin-left" rythm ]
            [ h1 [] [ text title ]
            , body
            ]
        ]


sidebar : Html msg
sidebar =
    let
        toSidebarLink name attrs =
            a
                (attrs
                    ++ [ style "border" "1px solid black"
                       , padding
                       ]
                )
                [ text name ]
    in
    [ ( "Homepage", Route.Index )
    , ( "Blog index", Route.Blog )
    , ( "All tags", Route.Blog__Tags )
    ]
        |> List.map
            (\( name, route ) ->
                Route.toLink (toSidebarLink name) route
            )
        |> column [ class "spaced" ]


padding : Attribute msg
padding =
    style "padding" rythm


rythm : String
rythm =
    ".5rem"


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    div
        (style "display" "flex"
            :: style "flex-direction" "column"
            :: attrs
        )
        children


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    div
        (style "display" "flex"
            :: attrs
        )
        children


priorityBadge : Int -> Html msg
priorityBadge priority =
    let
        color =
            priorityToColor priority
    in
    div
        [ style "background-color" <| Color.toCssString color
        , style "color" <| Color.toCssString <| fontColor color
        , style "padding" "0.1rem"
        , style "display" "inline-block"
        ]
        [ Html.text <| String.fromInt priority ]


fontColor : Color -> Color
fontColor color =
    let
        { red, green, blue } =
            Color.toRgba color

        luminance =
            0.299 * red + 0.587 * green + 0.114 * blue
    in
    if luminance > 0.5 then
        Color.black

    else
        Color.white


priorityToColor : Int -> Color
priorityToColor priority =
    let
        hue =
            toFloat priority / 100 * 2 / 3
    in
    Color.hsl hue 1 0.75
