module Page.Slug_ exposing (Data, Model, Msg, page)

import Data.Article as Article exposing (ArticleMetadata, LightArticle(..))
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
    { content : String
    , isMarkdown : Bool
    , metadata : ArticleMetadata
    }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , data = data
        , routes =
            Article.list
                |> DataSource.map
                    (List.concatMap getSlugs)
        }
        |> Page.buildNoState { view = view }


getSlugs : LightArticle -> List RouteParams
getSlugs article =
    case article of
        ArticleHtml s ->
            [ s ]

        ArticleMarkdown s ->
            [ s ]

        ArticleLink { slugs } ->
            slugs
                |> List.map (\slug -> { slug = slug })


data : RouteParams -> DataSource Data
data { slug } =
    Article.slugToFilePath slug
        |> DataSource.andThen
            (\{ path, isMarkdown } ->
                DataSource.File.bodyWithFrontmatter
                    (\content ->
                        Decode.map
                            (\metadata ->
                                { content = content
                                , isMarkdown = isMarkdown
                                , metadata = metadata
                                }
                            )
                            Article.metadataDecoder
                    )
                    path
            )


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
view _ _ static =
    { title = Just <| static.data.metadata.title ++ " (" ++ String.fromInt static.data.metadata.priority ++ ")"
    , body = ArticleBody static.data
    }
