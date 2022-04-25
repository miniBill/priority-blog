module Theme exposing (layout, priorityBadge)

import Color exposing (Color)
import Data.Route
import Data.Tag as Tag exposing (Tag)
import Html as H exposing (Html)
import Html.Attributes as HA
import Route exposing (Route)
import View exposing (View)


layout : { tags : List ( Tag, Int ) } -> View msg -> List (Html msg) -> Html msg
layout tags view body =
    let
        title =
            if String.isEmpty view.title then
                H.text ""

            else
                H.h1 [] [ H.text view.title ]
    in
    H.main_ []
        [ sidebar tags
        , H.article [] (title :: body)
        ]


sidebar : { tags : List ( Tag, Int ) } -> Html msg
sidebar tags =
    let
        logo =
            Route.toLink H.a
                Route.Index
                [ H.img
                    [ HA.class "logo"
                    , HA.src "/logo.svg"
                    ]
                    []
                ]
    in
    H.nav [] [ logo, tagCloud tags ]


tagCloud : { tags : List ( Tag, Int ) } -> Html msg
tagCloud { tags } =
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

        toLink ( tag, count ) =
            Route.toLink
                (\attrs ->
                    let
                        ( size, weight ) =
                            tagSizeAndWeight count
                    in
                    H.a attrs
                        [ H.span
                            [ HA.style "font-size" <| String.fromInt size ++ "px"
                            , HA.style "font-weight" <| String.fromInt weight
                            ]
                            [ H.text <| Tag.name tag ]
                        ]
                )
                (Route.Tags__Slug_ { slug = Tag.toSlug tag })

        cloud =
            tags
                |> List.sortBy (Tuple.second >> negate)
                |> List.map toLink
                |> H.div [ HA.id "tag-cloud" ]
    in
    H.section []
        [ H.h2 [] [ Route.toLink H.a Route.Tags [ H.text Data.Route.routeLabels.tags ] ]
        , cloud
        ]


priorityBadge : Int -> Html msg
priorityBadge priority =
    let
        color =
            priorityToColor priority
    in
    H.div
        [ HA.style "background-color" <| Color.toCssString color
        , HA.style "color" <| Color.toCssString <| fontColor color
        , HA.style "padding" "0.1rem"
        , HA.style "display" "inline-block"
        ]
        [ H.text <| String.fromInt priority ]


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
