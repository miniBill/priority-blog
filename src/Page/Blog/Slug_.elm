module Page.Blog.Slug_ exposing (Data, Model, Msg, page)

import Data.Article as Article exposing (ArticleMetadata)
import DataSource exposing (DataSource)
import DataSource.File
import Head
import Head.Seo as Seo
import OptimizedDecoder as Decode
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import View exposing (Body(..), View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { slug : String
    }


type alias Data =
    { markdown : String
    , metadata : ArticleMetadata
    }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , data = data
        , routes = Article.list
        }
        |> Page.buildNoState { view = view }


data : RouteParams -> DataSource Data
data { slug } =
    DataSource.File.bodyWithFrontmatter
        (\markdown ->
            Decode.map
                (\metadata ->
                    { markdown = markdown
                    , metadata = metadata
                    }
                )
                Article.metadataDecoder
        )
        (Article.slugToFilePath slug)


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = static.data.metadata.title
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.metadata.title ++ " (" ++ String.fromInt static.data.metadata.priority ++ ")"
    , body = ArticleBody static.data
    }
