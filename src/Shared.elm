module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template, viewTag)

import Browser.Navigation
import DataSource
import Html exposing (Html)
import Markdown.Parser
import Markdown.Renderer
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Theme
import View exposing (ArticleData, Body(..), View)


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
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMobileMenu : Bool
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
init navigationKey flags maybePagePath =
    ( { showMobileMenu = False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }, Cmd.none )

        SharedMsg globalMsg ->
            ( model, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


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
view sharedData page model toMsg pageView =
    { body = Theme.layout pageView.title <| viewToHtml pageView
    , title = pageView.title
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
        |> Result.map
            (\body ->
                body
            )


viewTag : String -> Html msg
viewTag tag =
    Route.Blog__Tags__Slug_ { slug = tag }
        |> Route.toLink (\attrs -> Html.a attrs [ Html.text tag ])
