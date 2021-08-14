module Page.Index exposing (Data, Model, Msg, page)

import Data.Article exposing (Article)
import DataSource exposing (DataSource)
import DateFormat
import Head
import Head.Seo as Seo
import Html exposing (a, b, h2, li, text, ul)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route
import Shared
import Time exposing (Posix)
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
    List ( Posix, Article )


data : DataSource Data
data =
    Data.Article.listWithMetadata
        |> DataSource.map (List.filterMap (\({ metadata } as article) -> Maybe.map (\datePublished -> ( datePublished, article )) metadata.datePublished))


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
    { breadcrumbs = []
    , title = "Homepage"
    , body =
        static.data
            |> List.sortBy (Tuple.first >> Time.posixToMillis >> negate)
            |> List.map
                (\( datePublished, { slug, metadata } ) ->
                    Route.toLink
                        (\attrs ->
                            a attrs
                                [ li []
                                    [ h2 [] [ text metadata.title ]
                                    , text "Published: "
                                    , b []
                                        [ text <|
                                            DateFormat.format
                                                [ DateFormat.monthNameAbbreviated
                                                , DateFormat.text " "
                                                , DateFormat.dayOfMonthSuffix
                                                , DateFormat.text ", "
                                                , DateFormat.yearNumber
                                                ]
                                                (Maybe.withDefault Time.utc sharedModel.here)
                                                datePublished
                                        ]
                                    ]
                                ]
                        )
                        (Route.Blog__Slug_ { slug = slug })
                )
            |> ul []
            |> HtmlBody
    }
