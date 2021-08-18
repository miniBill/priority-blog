module Data.Route exposing (routeToLabel)

import Route exposing (Route(..))


routeToLabel : Route -> Maybe String
routeToLabel route =
    case route of
        Index ->
            Just "Homepage"

        Blog ->
            Just "Blog index (by priority)"

        Blog__Slug_ _ ->
            Nothing

        Tags ->
            Just "All tags"

        Tags__Slug_ { slug } ->
            Just <| "Tag: " ++ slug
