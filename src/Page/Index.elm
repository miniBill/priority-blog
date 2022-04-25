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
    | Article { slug : String, description : Maybe String }


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
        link description attrs =
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
                ([ Theme.priorityBadge item.priority
                 , H.text " "
                 , H.a
                    attrs
                    [ H.text item.title ]
                 , H.text " - "
                 , tags
                 ]
                    ++ (case description of
                            Nothing ->
                                []

                            Just d ->
                                [ H.br [] []
                                , H.text d
                                ]
                       )
                )
    in
    case item.page of
        Article { slug, description } ->
            Route.toLink (link description) (Route.Article_ { article = slug })

        Link url ->
            link Nothing [ HA.href url ]


data : DataSource Data
data =
    Data.Article.listWithMetadata
        |> DataSource.map (List.filterMap articleToItem)
        -- TODO: distill
        |> identity


articleToItem : ArticleWithMetadata -> Maybe Item
articleToItem article =
    case article of
        ArticleFileWithMetadata { slug, metadata } ->
            Just
                { title = metadata.title
                , tags = metadata.tags
                , priority = metadata.priority
                , page = Article { slug = slug, description = metadata.description }
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
