module Page.Blog exposing (Data, Item, Model, Msg, articleToItem, page, viewArticleList)

import Data.Article exposing (ArticleWithMetadata(..))
import Data.Route
import Data.Tag exposing (Tag)
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as HA
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route exposing (Route)
import Shared
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
    , route : Route
    , tags : List Tag
    , title : String
    }


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
    { title = Data.Route.routeToLabel Route.Blog
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
    Route.toLink
        (\attrs ->
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
        )
        item.route


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
                , route = Route.Slug_ { slug = slug }
                }

        ArticleLinkWithMetadata { slug, metadata } ->
            Maybe.map2
                (\title priority ->
                    { title = title
                    , tags = metadata.tags
                    , priority = priority
                    , route = Route.Slug_ { slug = slug }
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
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "Blog index"
        }
        |> Seo.website
