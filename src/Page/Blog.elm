module Page.Blog exposing (Data, Model, Msg, page, viewArticleList)

import Data.Article exposing (Article)
import Data.Route
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
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
    { breadcrumbs = []
    , title = Data.Route.routeToLabel Route.Blog
    , body = viewArticleList static.data
    }


viewArticleList : List Article -> Body Msg
viewArticleList list =
    list
        |> List.map viewLink
        |> Theme.column [ Html.Attributes.class "spaced" ]
        |> HtmlBody


viewLink : Article -> Html Msg
viewLink article =
    Route.Blog__Slug_ { slug = article.slug }
        |> Route.toLink
            (\attrs ->
                let
                    tags =
                        List.map Shared.viewTag article.metadata.tags
                            |> List.intersperse (Html.text ", ")
                            |> Html.div
                                [ Html.Attributes.style "display" "inline-block"
                                , Html.Attributes.style "font-size" "0.8rem"
                                ]
                in
                Html.div []
                    [ Theme.priorityBadge article.metadata.priority
                    , Html.text " "
                    , Html.a
                        attrs
                        [ Html.text article.metadata.title ]
                    , Html.text " - "
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
