module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template, viewTag)

import Browser.Navigation
import Data.Article
import Data.Route
import Data.Tag as Tag exposing (Tag)
import DataSource exposing (DataSource)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Parser
import Html.Parser.Util
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
    { tags : List ( Tag, Int )
    }


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
        |> DataSource.map (\tags -> { tags = tags })


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
    { body = viewToHtml sharedData pageView
    , title =
        pageView.title
            |> Maybe.withDefault
                (route
                    |> Maybe.andThen Data.Route.routeToLabel
                    |> Maybe.withDefault "Incrium.com"
                )
    }


viewToHtml : Data -> View msg -> Html msg
viewToHtml sharedData pageView =
    case pageView.body of
        ArticleBody article ->
            Theme.layout sharedData pageView <|
                if article.isMarkdown then
                    case markdownToHtml article.content of
                        Ok content ->
                            content
                                ++ [ H.div [] <|
                                        H.text "Tags: "
                                            :: List.intersperse (H.text ", ")
                                                (List.map viewTag article.metadata.tags)
                                   ]

                        Err e ->
                            [ H.text e ]

                else
                    case Html.Parser.run article.content of
                        Ok content ->
                            Html.Parser.Util.toVirtualDom content
                                ++ [ H.div [] <|
                                        H.text "Tags: "
                                            :: List.intersperse (H.text ", ")
                                                (List.map viewTag article.metadata.tags)
                                   ]

                        Err _ ->
                            [ H.text "Error parsing HTML" ]

        MarkdownBody markdown ->
            Theme.layout sharedData pageView <|
                case markdownToHtml markdown of
                    Ok content ->
                        content

                    Err e ->
                        [ H.text e ]

        HtmlBody tags ->
            Theme.layout sharedData pageView tags

        RedirectBody url ->
            H.span []
                [ H.text "Redirecting to "
                , H.a [ HA.href url ] [ H.text url ]
                ]


markdownToHtml : String -> Result String (List (Html msg))
markdownToHtml markdown =
    let
        go m =
            m
                |> Markdown.Parser.parse
                |> Result.mapError (\_ -> "Markdown error.")
                |> Result.andThen
                    (\blocks ->
                        Markdown.Renderer.render
                            Markdown.Renderer.defaultHtmlRenderer
                            blocks
                    )

        toMarkdownFallbackText m =
            go m
                |> Result.withDefault [ H.text m ]

        smAttributeToRow attr =
            let
                trimmed =
                    String.trim attr

                cut =
                    if String.startsWith "\\#" trimmed then
                        String.dropLeft 2 trimmed

                    else
                        trimmed
            in
            case String.indexes ":" cut of
                [] ->
                    H.tr [] [ H.td [] [ H.text trimmed ] ]

                colon :: _ ->
                    H.tr []
                        [ H.th [] [ H.text <| String.trim <| String.left colon cut ]
                        , H.td [] <| toMarkdownFallbackText <| String.trim <| String.dropLeft (colon + 1) cut
                        ]

        smReferenceToTable tail =
            tail
                |> String.split "\n"
                |> List.map smAttributeToRow
                |> H.table [ HA.class "sm-table" ]
    in
    case String.split "**#SuperMemo Reference:****" markdown of
        [ m, tail ] ->
            go m
                |> Result.map
                    (\rendered ->
                        rendered ++ [ smReferenceToTable tail ]
                    )

        _ ->
            go markdown


viewTag : Tag -> Html msg
viewTag tag =
    Route.Tags__Slug_ { slug = Tag.toSlug tag }
        |> Route.toLink (\attrs -> H.a attrs [ H.text <| Tag.name tag ])
