module Page.Tags.Slug_ exposing (Data, Model, Msg, RouteParams, page)

import Data.Article as Article
import Data.Route as Route
import Data.Tag as Tag
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import List.Extra
import Page exposing (Page, StaticPayload)
import Page.Index as Blog
import Pages.PageUrl exposing (PageUrl)
import Serialize as Codec exposing (Codec)
import Shared
import Site
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { slug : String }


type alias Data =
    { tag : String
    , articles : List Blog.Item
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
                let
                    filteredArticles =
                        articles
                            |> List.filterMap Blog.articleToItem
                            |> List.filter
                                (\{ tags } ->
                                    List.any
                                        (\tag -> Tag.toSlug tag == routeParams.slug)
                                        tags
                                )

                    foundTag =
                        List.Extra.findMap
                            (\{ tags } ->
                                List.Extra.find
                                    (\tag -> Tag.toSlug tag == routeParams.slug)
                                    tags
                            )
                            filteredArticles
                in
                { tag = Maybe.withDefault routeParams.slug <| Maybe.map Tag.name foundTag
                , articles = filteredArticles
                }
            )
        |> DataSource.distillSerializeCodec ("tag-" ++ routeParams.slug) dataCodec


dataCodec : Codec () Data
dataCodec =
    Codec.record Data
        |> Codec.field .tag Codec.string
        |> Codec.field .articles (Codec.list itemCodec)
        |> Codec.finishRecord


itemCodec : Codec () Blog.Item
itemCodec =
    Codec.record
        (\priority page_ tags title ->
            { priority = priority
            , page = page_
            , tags = tags
            , title = title
            }
        )
        |> Codec.field .priority Codec.int
        |> Codec.field .page pageCodec
        |> Codec.field .tags Tag.listCodec
        |> Codec.field .title Codec.string
        |> Codec.finishRecord


pageCodec : Codec () Blog.LinkOrArticle
pageCodec =
    Codec.customType
        (\farticle flink value ->
            case value of
                Blog.Article { article } ->
                    farticle article

                Blog.Link url ->
                    flink url
        )
        |> Codec.variant1 (\article -> Blog.Article { article = article }) Codec.string
        |> Codec.variant1 Blog.Link Codec.string
        |> Codec.finishCustomType


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = Site.name
        , image = Site.logo
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
    { title = Route.routeLabels.tag static.data.tag
    , body = Blog.viewArticleList static.data.articles
    }
