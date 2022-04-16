module Page.Index exposing (Data, Model, Msg, page)

import Data.Article exposing (Article, ArticleTime(..))
import DataSource exposing (DataSource)
import Date
import DateFormat
import Head
import Head.Seo as Seo
import Html exposing (Html, a, b, h2, hr, li, text, ul)
import Html.Attributes exposing (style)
import Page exposing (Page, StaticPayload)
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
    List ( ArticleTime, Article )


data : DataSource Data
data =
    Data.Article.listWithMetadata
        |> DataSource.map
            (List.filterMap (\({ metadata } as article) -> Maybe.map (\datePublished -> ( datePublished, article )) metadata.datePublished)
                >> List.sortBy tupleToOrder
                >> List.take articlesInHomepage
            )


tupleToOrder : ( ArticleTime, Article ) -> ( Int, Int )
tupleToOrder ( datePublished, article ) =
    ( negate <|
        -- TODO: Make this more sensibe
        case datePublished of
            Iso8601 p ->
                Date.toRataDie <| Date.fromPosix Time.utc p

            Date d ->
                Date.toRataDie d
    , article.metadata.priority
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
            |> ul [ style "width" "100%" ]
            |> (\l -> HtmlBody [ l ])
    }


viewArticle : Shared.Model -> (( ArticleTime, Article ) -> Html Msg)
viewArticle sharedModel ( datePublished, { slug, metadata } ) =
    let
        inner attrs =
            a
                (attrs
                    ++ [ Html.Attributes.style "text-decoration" "none"
                       , Html.Attributes.style "color" "black"
                       ]
                )
                [ li []
                    [ h2 [] [ text metadata.title ]
                    , text "Published: "
                    , b []
                        [ text <|
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
    Route.toLink inner <| Route.Blog__Slug_ { slug = slug }


separator : Html.Html Msg
separator =
    li [ style "width" "100%" ]
        [ hr [ style "width" "100%" ] []
        ]
