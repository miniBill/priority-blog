module View exposing (ArticleData, Body(..), View, map, placeholder)

import Data.Article exposing (ArticleMetadata)
import Html as H exposing (Html)


type Body msg
    = HtmlBody (List (Html msg))
    | ArticleBody ArticleData
    | RedirectBody String


type alias ArticleData =
    { isMarkdown : Bool
    , content : String
    , metadata : ArticleMetadata
    }


type alias View msg =
    { title : String
    , body : Body msg
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body =
        case doc.body of
            HtmlBody body ->
                HtmlBody <| List.map (H.map fn) body

            ArticleBody article ->
                ArticleBody article

            RedirectBody url ->
                RedirectBody url
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = ""
    , body = HtmlBody [ H.text moduleName ]
    }
