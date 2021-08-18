module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template, viewTag)

import Browser.Navigation
import Data.Article
import Data.Route
import DataSource exposing (DataSource)
import Html exposing (Html)
import Markdown.Parser
import Markdown.Renderer
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Task
import Theme
import Time
import View exposing (Body(..), View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg


type alias Data =
    List ( String, Int )


type SharedMsg
    = Here Time.Zone


type alias Model =
    { here : Maybe Time.Zone
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init _ _ _ =
    ( { here = Nothing }
    , Task.perform (SharedMsg << Here) Time.here
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( model, Cmd.none )

        SharedMsg (Here here) ->
            ( { model | here = Just here }, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource Data
data =
    Data.Article.tags


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view sharedData { route } _ _ pageView =
    { body = Theme.layout sharedData pageView <| viewToHtml pageView
    , title =
        pageView.title
            |> Maybe.withDefault
                (route
                    |> Maybe.andThen Data.Route.routeToLabel
                    |> Maybe.withDefault "TODO"
                )
    }


viewToHtml : View msg -> Html msg
viewToHtml pageView =
    case pageView.body of
        ArticleBody article ->
            case markdownToHtml article.markdown of
                Ok content ->
                    Html.div []
                        (content
                            ++ [ Html.div [] <|
                                    Html.text "Tags: "
                                        :: List.intersperse (Html.text ", ")
                                            (List.map viewTag article.metadata.tags)
                               ]
                        )

                Err e ->
                    Html.text e

        MarkdownBody markdown ->
            case markdownToHtml markdown of
                Ok content ->
                    Html.div [] content

                Err e ->
                    Html.text e

        HtmlBody tag ->
            tag


markdownToHtml : String -> Result String (List (Html msg))
markdownToHtml markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\_ -> "Markdown error.")
        |> Result.andThen
            (\blocks ->
                Markdown.Renderer.render
                    Markdown.Renderer.defaultHtmlRenderer
                    blocks
            )


viewTag : String -> Html msg
viewTag tag =
    Route.Tags__Slug_ { slug = tag }
        |> Route.toLink (\attrs -> Html.a attrs [ Html.text tag ])
