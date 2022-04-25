module Data.Route exposing (routeLabels, routeToLabel)

import Route exposing (Route(..))


routeToLabel : Route -> Maybe String
routeToLabel route =
    case route of
        Index ->
            Just routeLabels.index

        BlogByDate ->
            Just routeLabels.blogByDate

        Article_ _ ->
            Nothing

        Tags ->
            Just routeLabels.tags

        Tags__Slug_ { slug } ->
            Just <| routeLabels.tag slug


routeLabels :
    { blogByDate : String
    , index : String
    , tags : String
    , tag : String -> String
    }
routeLabels =
    { blogByDate = "Blog (by date)"
    , index = "Homepage"
    , tags = "All tags"
    , tag = \slug -> "Tag: " ++ slug
    }
