module Page.Index exposing (Data, Model, Msg, page)

import Data.Article exposing (ArticleTime(..), ArticleWithMetadata(..))
import DataSource exposing (DataSource)
import Date
import DateFormat
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as HA
import Page exposing (Page, StaticPayload)
import Page.Blog as Blog
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route
import Shared
import Time
import View exposing (Body(..), View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


type alias Data =
    List ( ArticleTime, Blog.Item )


data : DataSource Data
data =
    Data.Article.listWithMetadata
        |> DataSource.map
            (List.filterMap
                (\article ->
                    Maybe.map2 Tuple.pair
                        (getDatePublished article)
                        (Blog.articleToItem article)
                )
                >> List.sortBy tupleToOrder
                >> List.take articlesInHomepage
            )


getDatePublished : ArticleWithMetadata -> Maybe ArticleTime
getDatePublished article =
    case article of
        ArticleFileWithMetadata { metadata } ->
            metadata.datePublished

        ArticleLinkWithMetadata { metadata } ->
            metadata.datePublished


tupleToOrder : ( ArticleTime, Blog.Item ) -> ( Int, Int )
tupleToOrder ( datePublished, item ) =
    ( negate <|
        -- TODO: Make this more sensibe
        case datePublished of
            Iso8601 p ->
                Date.toRataDie <| Date.fromPosix Time.utc p

            Date d ->
                Date.toRataDie d
    , item.priority
    )


articlesInHomepage : Int
articlesInHomepage =
    5


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
        , title = "TODO"
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ sharedModel static =
    { title = Nothing
    , body =
        static.data
            |> List.map (viewArticle sharedModel)
            |> List.intersperse separator
            |> H.ul [ HA.style "width" "100%" ]
            |> (\l -> HtmlBody [ l ])
    }


viewArticle : Shared.Model -> (( ArticleTime, Blog.Item ) -> Html Msg)
viewArticle sharedModel ( datePublished, item ) =
    let
        inner attrs =
            H.a
                (attrs
                    ++ [ HA.style "text-decoration" "none"
                       , HA.style "color" "black"
                       ]
                )
                [ H.li []
                    [ H.h2 [] [ H.text item.title ]
                    , H.text "Published: "
                    , H.b []
                        [ H.text <|
                            case datePublished of
                                Iso8601 iso ->
                                    DateFormat.format
                                        [ DateFormat.monthNameAbbreviated
                                        , DateFormat.text " "
                                        , DateFormat.dayOfMonthSuffix
                                        , DateFormat.text ", "
                                        , DateFormat.yearNumber
                                        ]
                                        (Maybe.withDefault Time.utc sharedModel.here)
                                        iso

                                Date d ->
                                    Date.format "MMM ddd, y" d
                        ]
                    ]
                ]
    in
    case item.page of
        Blog.Article slug ->
            Route.toLink inner (Route.Article_ slug)

        Blog.Link url ->
            inner [ HA.href url ]


separator : Html Msg
separator =
    H.li [ HA.style "width" "100%" ]
        [ H.hr [ HA.style "width" "100%" ] []
        ]
