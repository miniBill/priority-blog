module Page.Blog exposing (Data, Model, Msg, page, viewArticleList)

import Data.Article exposing (Article)
import Data.Route
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as HA
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route
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
    List Article


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


viewArticleList : List Article -> Body Msg
viewArticleList list =
    list
        |> List.sortBy (\{ metadata } -> metadata.priority)
        |> List.map viewLink
        |> HtmlBody


viewLink : Article -> Html Msg
viewLink article =
    Route.Slug_ { slug = article.slug }
        |> Route.toLink
            (\attrs ->
                let
                    tags =
                        List.map Shared.viewTag article.metadata.tags
                            |> List.intersperse (H.text ", ")
                            |> H.div
                                [ HA.style "display" "inline-block"
                                , HA.style "font-size" "0.8rem"
                                ]
                in
                H.div []
                    [ Theme.priorityBadge article.metadata.priority
                    , H.text " "
                    , H.a
                        attrs
                        [ H.text article.metadata.title ]
                    , H.text " - "
                    , tags
                    ]
            )


data : DataSource Data
data =
    Data.Article.listWithMetadata


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
