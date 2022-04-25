module Page.Index exposing (Data, Item, LinkOrArticle(..), Model, Msg, RouteParams, articleToItem, page, viewArticleList)

import Data.Article exposing (ArticleWithMetadata(..))
import Data.Tag exposing (Tag)
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as HA
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Route
import Shared
import Site
import Theme
import View exposing (Body(..), View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


type alias Data =
    List Item


type alias Item =
    { priority : Int
    , page : LinkOrArticle
    , tags : List Tag
    , title : String
    }


type LinkOrArticle
    = Link String
    | Article { article : String }


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    { title = ""
    , body = viewArticleList static.data
    }


viewArticleList : List Item -> Body Msg
viewArticleList list =
    list
        |> List.sortBy .priority
        |> List.map viewLink
        |> HtmlBody


viewLink : Item -> Html Msg
viewLink item =
    let
        link =
            \attrs ->
                let
                    tags =
                        List.map Shared.viewTag item.tags
                            |> List.intersperse (H.text ", ")
                            |> H.div
                                [ HA.style "display" "inline-block"
                                , HA.style "font-size" "0.8rem"
                                ]
                in
                H.div []
                    [ Theme.priorityBadge item.priority
                    , H.text " "
                    , H.a
                        attrs
                        [ H.text item.title ]
                    , H.text " - "
                    , tags
                    ]
    in
    case item.page of
        Article slug ->
            Route.toLink link (Route.Article_ slug)

        Link url ->
            link [ HA.href url ]


data : DataSource Data
data =
    Data.Article.listWithMetadata
        |> DataSource.map (List.filterMap articleToItem)


articleToItem : ArticleWithMetadata -> Maybe Item
articleToItem article =
    case article of
        ArticleFileWithMetadata { slug, metadata } ->
            Just
                { title = metadata.title
                , tags = metadata.tags
                , priority = metadata.priority
                , page = Article { article = slug }
                }

        ArticleLinkWithMetadata { url, metadata } ->
            Maybe.map2
                (\title priority ->
                    { title = title
                    , tags = metadata.tags
                    , priority = priority
                    , page = Link url
                    }
                )
                metadata.title
                metadata.priority


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = Site.name
        , image = Site.logo
        , description = Site.description
        , locale = Nothing
        , title = Site.name
        }
        |> Seo.website
