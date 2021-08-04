module Data.Route exposing (routeToLabel)

import Route exposing (Route(..))


routeToLabel : Route -> Maybe String
routeToLabel route =
    case route of
        Index ->
            Just "Homepage"

        Blog__Tags ->
            Just "All tags"

        Blog__Tags__Slug_ _ ->
            Nothing

        Blog__Slug_ _ ->
            Nothing

        Blog ->
            Just "Blog index"
