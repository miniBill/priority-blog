module Page.Tags exposing (Data, Model, Msg, page)

import Data.Article as Article
import Data.Route as Route
import Data.Tag as Tag exposing (Tag)
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as H
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route exposing (Route(..))
import Shared exposing (viewTag)
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
    List ( Tag, Int )


data : DataSource Data
data =
    Article.tags


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
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    { title = Route.routeToLabel Tags
    , body =
        static.data
            |> List.sortBy (Tuple.first >> Tag.toSlug)
            |> List.sortBy (Tuple.second >> negate)
            |> List.map
                (\( tag, count ) ->
                    H.div []
                        [ viewTag tag
                        , H.text <| " (" ++ String.fromInt count ++ ")"
                        ]
                )
            |> HtmlBody
    }
