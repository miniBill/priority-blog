module View exposing (ArticleData, Body(..), View, map, placeholder)

import Data.Article exposing (ArticleMetadata)
import Html as H exposing (Html)


type Body msg
    = HtmlBody (List (Html msg))
    | MarkdownBody String
    | ArticleBody ArticleData


type alias ArticleData =
    { isMarkdown : Bool
    , content : String
    , metadata : ArticleMetadata
    }


type alias View msg =
    { title : Maybe String
    , body : Body msg
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body =
        case doc.body of
            HtmlBody body ->
                HtmlBody <| List.map (H.map fn) body

            MarkdownBody mk ->
                MarkdownBody mk

            ArticleBody article ->
                ArticleBody article
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = Nothing
    , body = HtmlBody [ H.text moduleName ]
    }
