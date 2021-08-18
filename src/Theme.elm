module Theme exposing (column, layout, priorityBadge, row)

import Color exposing (Color)
import Data.Route
import Html exposing (Attribute, Html, a, div, h1, span, text)
import Html.Attributes exposing (class, style)
import Route exposing (Route)
import View exposing (View)


layout : List ( String, Int ) -> View msg -> Html msg -> Html msg
layout tags { title } body =
    row [ padding ]
        [ sidebar tags
        , div
            [ style "margin-left" rythm
            , style "width" "100%"
            ]
            [ case title of
                Just t ->
                    h1 [ style "display" "inline-block" ] [ text t ]

                Nothing ->
                    text ""
            , body
            ]
        ]


sidebar : List ( String, Int ) -> Html msg
sidebar tags =
    column [ class "spaced" ]
        [ routeButton Route.Index
        , routeButton Route.Blog
        , routeButton Route.Tags
        , tagCloud tags
        ]


tagCloud : List ( String, Int ) -> Html msg
tagCloud tags =
    let
        ( minCount, maxCount ) =
            case tags of
                [] ->
                    ( 0, 0 )

                ( _, i ) :: t ->
                    List.foldl (\( _, e ) ( mn, mx ) -> ( min mn e, max mx e )) ( i, i ) t

        minSize =
            14

        minWeight =
            400

        maxSize =
            18

        maxWeight =
            900

        scale mnf mxf mnt mxt value =
            if mnf == mxf then
                (mnt + mxt) // 2

            else
                (value - mnf) * (mxt - mnt) // (mxf - mnf) + mnt

        tagSizeAndWeight count =
            ( scale minCount maxCount minSize maxSize count
            , scale minCount maxCount minWeight maxWeight count
            )

        toLink ( slug, count ) =
            Route.toLink
                (\attrs ->
                    let
                        ( size, weight ) =
                            tagSizeAndWeight count
                    in
                    a attrs
                        [ span
                            [ style "font-size" <| String.fromInt size ++ "px"
                            , style "font-weight" <| String.fromInt weight
                            ]
                            [ text slug ]
                        ]
                )
                (Route.Tags__Slug_ { slug = slug })
    in
    tags
        |> List.sortBy (Tuple.second >> negate)
        |> List.map toLink
        |> List.intersperse (text " ")
        |> div []


routeButton : Route -> Html msg
routeButton route =
    let
        toSidebarLink name attrs =
            div [] [ text "∘ ", a attrs [ text name ] ]
    in
    Data.Route.routeToLabel route
        |> Maybe.map
            (\label ->
                Route.toLink
                    (toSidebarLink label)
                    route
            )
        |> Maybe.withDefault (text "")


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
        [ text <| String.fromInt priority ]


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
