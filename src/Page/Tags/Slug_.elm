module Page.Tags.Slug_ exposing (Data, Model, Msg, page)

import Data.Article as Article exposing (ArticleWithMetadata)
import Data.Tag as Tag
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Page exposing (Page, StaticPayload)
import Page.Blog
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Serialize as Codec exposing (Codec)
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { slug : String }


type alias Data =
    { tag : String
    , articles : List ArticleWithMetadata
    }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    DataSource.map
        (List.map <| \( tag, _ ) -> { slug = Tag.toSlug tag })
        Article.tags


data : RouteParams -> DataSource Data
data routeParams =
    Article.listWithMetadata
        |> DataSource.map
            (\articles ->
                { tag = routeParams.slug
                , articles =
                    articles
                        |> List.filter
                            (\{ metadata } ->
                                List.any
                                    (\tag -> Tag.toSlug tag == routeParams.slug)
                                    metadata.tags
                            )
                }
            )
        |> DataSource.distillSerializeCodec ("tag-" ++ routeParams.slug) dataCodec


dataCodec : Codec () Data
dataCodec =
    Codec.record Data
        |> Codec.field .tag Codec.string
        |> Codec.field .articles (Codec.list Article.codec)
        |> Codec.finishRecord


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
        , description = "Articles with tag: " ++ static.data.tag
        , locale = Nothing
        , title = static.data.tag
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    { title = Just <| "Tag: " ++ static.data.tag
    , body = Page.Blog.viewArticleList static.data.articles
    }
