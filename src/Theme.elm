module Theme exposing (column, layout, priorityBadge, row)

import Color exposing (Color)
import Data.Route
import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes exposing (class, style)
import Route exposing (Route)
import View exposing (View)


layout : List ( String, Int ) -> View msg -> Html msg -> Html msg
layout tags { title, breadcrumbs } body =
    row [ padding ]
        [ sidebar tags
        , div
            [ style "margin-left" rythm
            , style "width" "100%"
            ]
            [ div [] <|
                List.intersperse (Html.text " > ") <|
                    List.filterMap viewBreadcrumb breadcrumbs
                        ++ (case title of
                                Just t ->
                                    [ h1 [ style "display" "inline-block" ] [ text t ] ]

                                Nothing ->
                                    []
                           )
            , body
            ]
        ]


viewBreadcrumb : Route -> Maybe (Html msg)
viewBreadcrumb route =
    Data.Route.routeToLabel route
        |> Maybe.map
            (\label ->
                Route.toLink
                    (\attrs ->
                        a attrs
                            [ text label ]
                    )
                    route
            )


sidebar : List ( String, Int ) -> Html msg
sidebar tags =
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
    ([ Route.Index
     , Route.Blog
     , Route.Blog__Tags
     ]
        ++ (tags
                |> List.sortBy (Tuple.second >> negate)
                |> List.map
                    (\( tag, _ ) ->
                        Route.Blog__Tags__Slug_ { slug = tag }
                    )
           )
    )
        |> List.filterMap
            (\route ->
                Data.Route.routeToLabel route
                    |> Maybe.map
                        (\label ->
                            Route.toLink
                                (toSidebarLink label)
                                route
                        )
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
