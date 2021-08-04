module View exposing (ArticleData, Body(..), View, map, placeholder)

import Data.Article exposing (ArticleMetadata)
import Html exposing (Html)
import Route exposing (Route)
import Time exposing (Month(..))


type Body msg
    = HtmlBody (Html msg)
    | MarkdownBody String
    | ArticleBody ArticleData


type alias ArticleData =
    { markdown : String
    , metadata : ArticleMetadata
    }


type alias View msg =
    { breadcrumbs : List Route
    , title : String
    , body : Body msg
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { breadcrumbs = doc.breadcrumbs
    , title = doc.title
    , body =
        case doc.body of
            HtmlBody body ->
                HtmlBody <| Html.map fn body

            MarkdownBody mk ->
                MarkdownBody mk

            ArticleBody article ->
                ArticleBody article
    }


placeholder : String -> View msg
placeholder moduleName =
    { breadcrumbs = []
    , title = "Placeholder - " ++ moduleName
    , body = HtmlBody <| Html.text moduleName
    }
