module Theme exposing (layout, priorityBadge)

import Color exposing (Color)
import Data.Route
import Data.Tag as Tag exposing (Tag)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Route exposing (Route)
import View exposing (View)


layout : { tags : List ( Tag, Int ) } -> View msg -> List (Html msg) -> Html msg
layout tags view body =
    let
        title =
            case view.title of
                Just t ->
                    H.h1 [ HA.style "display" "inline-block" ] [ H.text t ]

                Nothing ->
                    H.text ""
    in
    row [ padding ]
        [ sidebar tags
        , H.article
            [ HA.style "margin-left" rythm
            , HA.style "width" "100%"
            , HA.style "max-width" "640px"
            ]
            (title :: body)
        ]


sidebar : { tags : List ( Tag, Int ) } -> Html msg
sidebar tags =
    H.aside [ HA.class "spaced" ]
        [ routeLink Route.Index
        , routeLink Route.Blog
        , routeLink Route.Tags
        , tagCloud tags
        ]


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
    in
    tags
        |> List.sortBy (Tuple.second >> negate)
        |> List.map toLink
        |> List.intersperse (H.text " ")
        |> H.div []


routeLink : Route -> Html msg
routeLink route =
    let
        toSidebarLink name attrs =
            H.div [] [ H.text "∘ ", H.a attrs [ H.text name ] ]
    in
    Data.Route.routeToLabel route
        |> Maybe.map
            (\label ->
                Route.toLink
                    (toSidebarLink label)
                    route
            )
        |> Maybe.withDefault (H.text "")


padding : Attribute msg
padding =
    HA.style "padding" rythm


rythm : String
rythm =
    ".5rem"


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    H.div
        (HA.style "display" "flex"
            :: attrs
        )
        children


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
